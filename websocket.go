package main

import (
	"crypto/sha1"
	"encoding/base64"
	"fmt"
	"io"
	"net/http"
)

func checksum(key string) string {
	magic := key + "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"
	h := sha1.New()
	io.WriteString(h, magic)
	sum := h.Sum(nil)
	b64 := base64.StdEncoding.EncodeToString(sum)

	fmt.Println("[DEBUG]", b64)
	return b64
}

func main() {
	checksum("dGhlIHNhbXBsZSBub25jZQ==")

	http.HandleFunc("/websocket", func(w http.ResponseWriter, r *http.Request) {
		protocols := r.Header.Get("Sec-WebSocket-Protocol")
		fmt.Println("[DEBUG]", protocols)

		key := r.Header.Get("Sec-WebSocket-Key")
		if key == "" {
			fmt.Println("[DEBUG]", "no key")
			w.WriteHeader(http.StatusBadRequest)
			w.Write(nil)
			return
		}
		w.Header().Set("Upgrade", "websocket")
		w.Header().Set("Connection", "Upgrade")
		w.Header().Set("Sec-WebSocket-Accept", checksum(key))

		w.WriteHeader(http.StatusOK)
		w.Write([]byte("101 Switching Protocols"))
	})

	http.ListenAndServe("localhost:8000", http.DefaultServeMux)
}
