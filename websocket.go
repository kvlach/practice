package main

import (
	"crypto/sha1"
	"encoding/base64"
	"errors"
	"fmt"
	"io"
	"net/http"
	"time"
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

func hijack(w http.ResponseWriter) {
	_, bufrw, err := http.NewResponseController(w).Hijack()
	if err != nil {
		panic(err)
	}

	time.Sleep(1 * time.Second)

	bufrw.WriteString("test")
	bufrw.Flush()

	read := make([]byte, 0, 1024)

	for {
		n, err := bufrw.Read(read)
		if err != nil && !errors.Is(err, io.EOF) {
			panic(err)
		}
		if n != 0 {
			fmt.Println(read[:n])
		}
		read = read[:0]
	}
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

		hijack(w)
	})

	http.ListenAndServe("localhost:8000", http.DefaultServeMux)
}
