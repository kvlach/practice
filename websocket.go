package main

import (
	"crypto/sha1"
	"encoding/base64"
	"fmt"
	"io"
	"net/http"
)

func main() {
	// req, err := http.NewRequest("GET", "localhost:8000", nil)
	// if err != nil {
	// 	panic(err)
	// }
	// req.Header.Set("Upgrade", "websocket")
	// req.Header.Set("Connection", "upgrade")
	// req.Header.Set("Sec-WebSocket-Key", "dGhlIHNhbXBsZSBub25jZQ==")
	// req.Header.Set("Sec-WebSocket-Version", "13")

	// resp, err := http.DefaultClient.Do(req)
	// if err != nil {
	// 	panic(err)
	// }
	// fmt.Println(resp.Header, resp)

	http.HandleFunc("/websocket", func(w http.ResponseWriter, r *http.Request) {
		fmt.Println(r)

		clientKey, ok := r.Header["Sec-WebSocket-Key"]
		if !ok {
			panic("expected Sec-WebSocket-Key")
		}
		magic := clientKey[0] + "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"
		h := sha1.New()
		io.WriteString(h, magic)
		sum := h.Sum(nil)
		// b64 :=
		encoder := base64.NewEncoder(base64.StdEncoding)

		w.Header().Set("Upgrade", "websocket")
		w.Header().Set("Connection", "upgrade")
		w.Header().Set("Sec-WebSocket-Accept", "s3pPLMBiTxaQ9kYGzzhZRbK+xOo=")

		w.WriteHeader(http.StatusOK)
		if _, err := w.Write([]byte{}); err != nil {
			panic(err)
		}
	})
	http.ListenAndServe("localhost:8000", http.DefaultServeMux)
}
