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
	return base64.StdEncoding.EncodeToString(h.Sum(nil))
}

func readWhole(bufrw *bufio.ReadWriter) []byte {
	const sz = 2
	whole := []byte{}
	tmp := make([]byte, sz)
	for {
		n, err := bufrw.Read(tmp)
		if errors.Is(err, io.EOF) {
			return whole
		}
		if err != nil {
			panic(err)
		}
		whole = append(whole, tmp[:n]...)
		if sz > n {
			return whole
		}
	}
}

func readDataFrame(df []byte) string {
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
		df = df[:8]
	}

	data := make([]byte, 0, len(df))
	if mask {
		maskingKey := df[:4]
		df = df[4:]
		for i, b := range df {
			data = append(data, b^maskingKey[i%4])
		}
	} else {
		data = df
	}

	return string(data)
}

func newDataFrame(msg string) []byte {
	df := make([]byte, 0, len(msg))

	df = append(df, 0b10000001)

	if len(msg) > 1<<16 {
		df = append(df, 127)
		binary.LittleEndian.AppendUint64(df[2:], uint64(len(msg)))
	} else if len(msg) > 125 {
		df = append(df, 126)
		binary.LittleEndian.AppendUint16(df[2:], uint16(len(msg)))
	} else {
		df = append(df, byte(len(msg)))
	}

	df = append(df, []byte(msg)...)
	fmt.Println(df)
	return df
}

func main() {
	http.HandleFunc("/websocket", func(w http.ResponseWriter, r *http.Request) {
		key := r.Header.Get("Sec-WebSocket-Key")
		if key == "" {
			w.WriteHeader(http.StatusBadRequest)
			panic("failed to find Sec-WebSocket-Key")
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

		for {
			df := readWhole(bufrw)
			fmt.Println(readDataFrame(df))

			bufrw.Write(newDataFrame("hi"))
			bufrw.Flush()
		}
	})

	http.ListenAndServe("localhost:8000", nil)
}
