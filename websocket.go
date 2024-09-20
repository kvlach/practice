package main

import (
	"crypto/sha1"
	"encoding/base64"
	"encoding/binary"
	"fmt"
	"io"
	"net"
	"net/http"
)

func acceptHeader(key string) string {
	magic := key + "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"
	h := sha1.New()
	io.WriteString(h, magic)
	sum := h.Sum(nil)
	b64 := base64.StdEncoding.EncodeToString(sum)

	fmt.Println("[DEBUG]", "Sec-WebSocket-Accept =", b64)
	return b64
}

type dataFrame struct {
	fin     bool
	rsv1    bool
	rsv2    bool
	rsv3    bool
	opcode  byte
	payload []byte
}

func dataFrameDecode(input []byte) []byte {
	// TODO: First byte

	mask := false
	if input[1]>>7 == 1 {
		mask = true
	}

	payloadLen := binary.LittleEndian.Uint64([]byte{input[1] & 0b01111111})
	input = input[:2]
	switch payloadLen {
	case 126:
		payloadLen = binary.LittleEndian.Uint64(input[:2])
		input = input[:2]
	case 127:
		payloadLen = binary.LittleEndian.Uint64(input[:8])
		input = input[:8]
	}

	var data []byte
	if mask {
		maskingKey := [4]byte(input[:4])
		input = input[:4]
		for i, b := range input {
			data = append(data, b^maskingKey[i%4])
		}
	} else {
		data = input
	}

	return data
}

func httpServer() {
	http.HandleFunc("/websocket", func(w http.ResponseWriter, r *http.Request) {
		fmt.Println("[DEBUG] Received request", r)

		key := r.Header.Get("Sec-WebSocket-Key")
		if key == "" {
			fmt.Println("[ERROR] No key")
			w.WriteHeader(http.StatusBadRequest)
			return
		}

		w.Header().Set("Connection", "Upgrade")
		w.Header().Set("Upgrade", "websocket")
		w.Header().Set("Sec-WebSocket-Accept", acceptHeader(key))

		w.WriteHeader(http.StatusSwitchingProtocols)
	})

	http.ListenAndServe("localhost:8000", http.DefaultServeMux)
}

func tcpServerHelper(conn net.Conn) {
	for {
		buf := make([]byte, 0, 4096)
		n, err := conn.Read(buf)
		if err != nil {
			fmt.Println("[ERROR] [TCP] Failed to read TCP:", err)
			continue
		}
		if n == 4096 {
			fmt.Println("[ERROR] [TCP] Read too much data")
			continue
		}
		fmt.Println(string(dataFrameDecode(buf)))
	}
}

func tcpServer() {
	ln, err := net.Listen("tcp", "localhost:8000")
	if err != nil {
		panic(err)
	}

	for {
		conn, err := ln.Accept()
		if err != nil {
			fmt.Println("[ERROR] [TCP] Failed to accept connection:", err)
			continue
		}
		go tcpServerHelper(conn)
	}
}

func main() {
	go httpServer()
	tcpServer()
}
