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
	"time"
)

func acceptHeader(key string) string {
	h := sha1.New()
	io.WriteString(h, key+"258EAFA5-E914-47DA-95CA-C5AB0DC85B11")
	return base64.StdEncoding.EncodeToString(h.Sum(nil))
}

func readWhole(bufrw *bufio.ReadWriter) []byte {
	const tmpBufferSize = 2
	read := []byte{}
	tmp := make([]byte, tmpBufferSize)
	for {
		n, err := bufrw.Read(tmp)
		if errors.Is(err, io.EOF) {
			return append(read, tmp[:n]...)
		}
		if err != nil {
			panic(err)
		}
		read = append(read, tmp[:n]...)
		if tmpBufferSize > n {
			return read
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

func reader(bufrw *bufio.ReadWriter) {
	for {
		read := readWhole(bufrw)
		fmt.Println(readDataFrame(read))
	}
}

func makeDataFrame(msg string) []byte {
	df := []byte{0b10000001}

	tmp := make([]byte, 2)
	binary.LittleEndian.PutUint16(tmp, uint16(len(msg)))

	df = append(df, tmp...)
	df = append(df, []byte(msg)...)

	return df
}

func main() {
	fmt.Println(readDataFrame(makeDataFrame("hello from the server")))
	http.HandleFunc("/websocket", func(w http.ResponseWriter, r *http.Request) {
		key := r.Header.Get("Sec-WebSocket-Key")
		if key == "" {
			panic("failed to get Sec-WebSocket-Key")
		}

		w.Header().Set("Connection", "Upgrade")
		w.Header().Set("Upgrade", "websocket")
		w.Header().Set("Sec-WebSocket-Accept", acceptHeader(key))

		w.WriteHeader(http.StatusSwitchingProtocols)

		hj, ok := w.(http.Hijacker)
		if !ok {
			panic("failed to cost to http.Hijacker")
		}
		_, bufrw, err := hj.Hijack()
		if err != nil {
			panic(err)
		}

		go reader(bufrw)

		time.Sleep(100 * time.Millisecond)
		bufrw.Write(makeDataFrame("hello from the server"))
		bufrw.Flush()
	})

	http.ListenAndServe("localhost:8000", nil)
}
