package main

import (
	"crypto/sha1"
	"encoding/base64"
	"encoding/binary"
	"errors"
	"fmt"
	"io"
	"net/http"
)

func acceptHeader(key string) string {
	magic := key + "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"
	h := sha1.New()
	io.WriteString(h, magic)
	sum := h.Sum(nil)
	b64 := base64.StdEncoding.EncodeToString(sum)

	fmt.Println("[DEBUG] Sec-WebSocket-Accept =", b64)
	return b64
}

func decodeDataFrame(df []byte) []byte {
	mask := false
	if df[0]&0b1 == 1 {
		mask = true
	}
	var payloadLen uint64
	payloadLen = uint64(df[1] & 0b01111111)
	df = df[2:]

	switch payloadLen {
	case 126:
		payloadLen = uint64(binary.LittleEndian.Uint16(df[:2]))
		df = df[2:]
	case 127:
		payloadLen = binary.LittleEndian.Uint64(df[:8])
		df = df[8:]
	}

	fmt.Println("[DEBUG] Payload Length =", payloadLen)

	decoded := make([]byte, 0, 1024)
	if mask {
		maskingKey := df[:4]
		fmt.Println("[DEBUG] Masking Key =", maskingKey)
		df = df[4:]
		fmt.Println(df)
		for i := 0; i < int(payloadLen); i++ {
			decoded = append(decoded, df[i]^maskingKey[i%4])
		}
		// for i, b := range df[:payloadLen] {
		// 	decoded = append(decoded, b^maskingKey[i%4])
		// }
	} else {
		decoded = df
	}

	return decoded
}

func main() {
	http.HandleFunc("/websocket", func(w http.ResponseWriter, r *http.Request) {
		key := r.Header.Get("Sec-WebSocket-Key")
		if key == "" {
			w.WriteHeader(http.StatusBadRequest)
			return
		}

		w.Header().Set("Connection", "Upgrade")
		w.Header().Set("Upgrade", "websocket")
		w.Header().Set("Sec-WebSocket-Accept", acceptHeader(key))

		w.WriteHeader(http.StatusSwitchingProtocols)

		hj, ok := w.(http.Hijacker)
		if !ok {
			panic("failed to cast to http.Hijacker")
		}

		_, bufrw, err := hj.Hijack()
		if err != nil {
			panic(err)
		}

		read := make([]byte, 1024)
		for {
			n, err := bufrw.Read(read)
			if errors.Is(err, io.EOF) {
				break
			}
			if err != nil {
				fmt.Println("[ERROR] failed to read buffer:", err)
				break
			}
			if n != 0 {
				decoded := decodeDataFrame(read[:n])
				fmt.Println(string(decoded), decoded)
			}
			read = read[:0]
		}
	})

	if err := http.ListenAndServe("localhost:8000", nil); err != nil {
		panic(err)
	}
}
