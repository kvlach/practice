package main

import (
	"bufio"
	"fmt"
	"net"
	"os"
	"strings"
)

const ADDR = "localhost:1234"

func readLine(conn net.Conn) (string, error) {
	return bufio.NewReader(conn).ReadString('\n')
}

func awaitOk(conn net.Conn) {
	data, err := readLine(conn)
	if err != nil {
		panic(err)
	}
	if data != "ok\n" {
		panic("there was an error: " + data)
	}
}

func cmdSend(conn net.Conn, message string) {
	fmt.Fprintf(conn, "SEND %s\n", message)
	awaitOk(conn)
}

func client() {
	conn, err := net.Dial("tcp", ADDR)
	if err != nil {
		panic(err)
	}
	defer conn.Close()

	cmdSend(conn, "Hello world!")
}

////////////
// SERVER //
////////////

var clients []net.Conn

func serverBroadcast(message string) {
	for _, cl := range clients {
		fmt.Fprint(cl, message)
	}
}

func serverHelper(conn net.Conn) {
	for {
		data, err := readLine(conn)
		if err != nil {
			panic(err)
		}

		if strings.HasPrefix(data, "SEND ") {
			serverBroadcast(strings.TrimPrefix(data, "SEND "))
		} else {
			fmt.Fprintf(conn, "unrecognized command")
		}
	}
}

func server() {
	ln, err := net.Listen("tcp", ADDR)
	if err != nil {
		panic(err)
	}

	for {
		conn, err := ln.Accept()
		if err != nil {
			panic(err)
		}
		fmt.Println("[SERVER] [DEBUG] Received client")
		go serverHelper(conn)
	}
}

func main() {
	if len(os.Args) != 2 {
		panic("Expected only one argument")
	}

	switch os.Args[1] {
	case "client":
		client()
	case "server":
		server()
	default:
		panic("Expected one of 'client', 'server' as an argument")
	}
}
