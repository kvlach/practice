package main

import (
	"crypto/sha1"
	"encoding/base64"
	"encoding/binary"
	"fmt"
	"io"
	"net/http"
)

func calcAcceptHeader(key string) string {
	magic := key + "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"
	h := sha1.New()
	io.WriteString(h, magic)
	sum := h.Sum(nil)
	b64 := base64.StdEncoding.EncodeToString(sum)

	fmt.Println("[DEBUG]", "Sec-WebSocket-Accept =", b64)
	return b64
}

type dataFrame struct {
	fin        bool
	rsv1       bool
	rsv2       bool
	rsv3       bool
	opcode     byte
	mask       bool
	payloadLen uint64
	maskKey    uint32
	data       []byte
}

func getMaskByte(mask uint32, n int) byte {
	return byte((mask >> n) & 0xff)
}

func newDataFrame(bs []byte) dataFrame {
	fin := false
	if bs[0]>>7 == 1 {
		fin = true
	}

	rsv1 := false
	if (bs[0]>>6)&0b1 == 1 {
		rsv1 = true
	}

	rsv2 := false
	if (bs[0]>>5)&0b1 == 1 {
		rsv2 = true
	}

	rsv3 := false
	if (bs[0]>>4)&0b1 == 1 {
		rsv3 = true
	}

	mask := false
	if bs[1]>>7 == 1 {
		mask = true
	}

	rawLen := bs[1] & 0b01111111
	var payloadLen uint64
	payloadLen = binary.LittleEndian.Uint64([]byte{rawLen})
	if payloadLen == 126 {
		payloadLen = binary.LittleEndian.Uint64(bs[2:4])
		bs = bs[:4]
	} else if payloadLen == 127 {
		payloadLen = binary.LittleEndian.Uint64(bs[2:10])
		bs = bs[:10]
	} else {
		bs = bs[:2]
	}

	var maskKey uint32
	if mask {
		maskKey = binary.LittleEndian.Uint32(bs[:4])
		bs = bs[:4]
	}

	data := make([]byte, 0, len(bs))
	for i, b := range bs {
		data = append(data, b^getMaskByte(maskKey, i%4))
	}

	return dataFrame{
		fin:        fin,
		rsv1:       rsv1,
		rsv2:       rsv2,
		rsv3:       rsv3,
		opcode:     bs[0] & 0b1111,
		mask:       mask,
		payloadLen: payloadLen,
		maskKey:    maskKey,
		data:       data,
	}
}

func main() {
	calcAcceptHeader("dGhlIHNhbXBsZSBub25jZQ==")

	http.HandleFunc("/websocket", func(w http.ResponseWriter, r *http.Request) {
		key := r.Header.Get("Sec-WebSocket-Key")
		if key == "" {
			w.WriteHeader(http.StatusBadRequest)
			return
		}

		w.Header().Set("Connection", "Upgrade")
		w.Header().Set("Upgrade", "websocket")
		w.Header().Set("Sec-WebSocket-Accept", calcAcceptHeader(key))

		w.WriteHeader(http.StatusSwitchingProtocols)
	})

	http.ListenAndServe("localhost:8000", http.DefaultServeMux)
}
