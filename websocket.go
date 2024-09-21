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

		// time.Sleep(4 * time.Second)
		// w.Write([]byte("is the connection alive?"))

		conn, _, err := http.NewResponseController(w).Hijack()
		if err != nil {
			panic(err)
		}

		for {
			buf := make([]byte, 0, 1024)
			_, err := conn.Read(buf)
			if err != nil && !errors.Is(err, io.EOF) {
				fmt.Println("[ERROR]", err)
				return
			}
			fmt.Println(buf)
			// buf = []byte{}

			// _, err = r.Response.Body.Read(buf)
			// if err != nil && !errors.Is(err, io.EOF) {
			// 	fmt.Println("[ERROR]", err)
			// 	return
			// }
			// fmt.Println(buf)

			time.Sleep(100 * time.Millisecond)
		}
	})

	http.ListenAndServe("localhost:8000", http.DefaultServeMux)
}
