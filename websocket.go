package main

import (
	"crypto/sha1"
	"encoding/base64"
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

	fmt.Println("[DEBUG]", b64)
	return b64
}

type firstByte struct {
	fin    bool
	rsv1   bool
	rsv2   bool
	rsv3   bool
	opcode byte // 4 bits
}

func newFirstByte(b byte) firstByte {
	fin := false
	if b>>7 == 1 {
		fin = true
	}

	rsv1 := false
	if b>>7 == 1 {
		rsv1 = true
	}

	rsv2 := false
	if b>>7 == 1 {
		rsv2 = true
	}

	rsv3 := false
	if b>>7 == 1 {
		rsv3 = true
	}

	return firstByte{
		fin:    fin,
		rsv1:   rsv1,
		rsv2:   rsv2,
		rsv3:   rsv3,
		opcode: b >> 4,
	}
}

func payloadLen(length [9]byte) {
	part1 := length[0] & 0b01111111

}

type dataFrame struct {
	firstByte  firstByte
	mask       bool
	length     [9]byte
	maskingKey int32
	payload    []byte
}

func newDataFrame(data []byte) dataFrame {
	mask := false
	if data[1]>>7 == 1 {
		mask = true
	}

	return dataFrame{
		firstByte: newFirstByte(data[0]),
		mask:      mask,
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

		w.Header().Set("Upgrade", "websocket")
		w.Header().Set("Connection", "Upgrade")
		w.Header().Set("Sec-WebSocket-Accept", calcAcceptHeader(key))

		w.WriteHeader(http.StatusSwitchingProtocols)
	})

	http.ListenAndServe("localhost:8000", http.DefaultServeMux)
}
