package main

import (
	"bufio"
	"crypto/sha1"
	"encoding/base64"
	"encoding/binary"
	"errors"
	"fmt"
	"io"
	"net/http"
)

func acceptHeader(key string) string {
	h := sha1.New()
	io.WriteString(h, key+"258EAFA5-E914-47DA-95CA-C5AB0DC85B11")
	sum := h.Sum(nil)
	b64 := base64.StdEncoding.EncodeToString(sum)

	fmt.Println("[DEBUG] Sec-WebSocket-Accept =", b64)
	return b64
}

func decodeDataFrame(df []byte) []byte {
	mask := false
	if df[0]>>7 == 1 {
		mask = true
	}

	payloadLen := uint64(df[1] & 0b01111111)
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

	data := make([]byte, 0, 1024)
	if mask {
		maskingKey := df[:4]
		fmt.Println("[DEBUG] Masking Key =", maskingKey)
		df = df[4:]
		for i, b := range df {
			data = append(data, b^maskingKey[i%4])
		}
	} else {
		data = df
	}

	return data
}

func bufRead(buf *bufio.ReadWriter) ([]byte, bool) {
	const sz = 2
	read := []byte{}
	tmp := make([]byte, sz)
	for {
		n, err := buf.Read(tmp)
		if errors.Is(err, io.EOF) {
			fmt.Println("[DEBUG] EOF")
			read = append(read, tmp[:n]...)
			return read, true
		}
		if err != nil {
			panic(err)
		}
		if n == 0 {
			break
		}
		read = append(read, tmp[:n]...)
		if n < sz {
			break
		}
	}
	return read, false
}

func main() {
	http.HandleFunc("/websocket", func(w http.ResponseWriter, r *http.Request) {
		key := r.Header.Get("Sec-WebSocket-Key")
		if key == "" {
			fmt.Println("[ERROR] Sec-WebSocket-Key not found")
			w.WriteHeader(http.StatusBadRequest)
			return
		}

		w.Header().Set("Connection", "Upgrade")
		w.Header().Set("Upgrade", "websocket")
		w.Header().Set("Sec-WebSocket-Accept", acceptHeader(key))

		w.WriteHeader(http.StatusSwitchingProtocols)

		hj, ok := w.(http.Hijacker)
		if !ok {
			panic("failed to cast to hijacker")
		}

		conn, bufrw, err := hj.Hijack()
		if err != nil {
			panic(err)
		}

		// rbuf := make([]byte, 1024)
		for {
			rbuf, eof := bufRead(bufrw)
			if eof {
				conn.Close()
				return
			}
			if err != nil {
				fmt.Println("[ERROR] Failed to read:", err)
				return
			}
			if len(rbuf) != 0 {
				decoded := decodeDataFrame(rbuf)
				fmt.Println(string(decoded), decoded)
			}
		}
	})

	http.ListenAndServe("localhost:8000", nil)
}
