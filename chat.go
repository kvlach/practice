package main

import (
	"bufio"
	"fmt"
	"net"
	"os"
)

const ADDR = "localhost:1234"

const CMD_SEND = "SEND "
const CMD_RECV = "RECV "

func whichCmd(s string) (string, string) {
	if len(s) < 6 {
		panic("expected at least 6 (cmd + space + newline) characters")
	}
	if s[len(s)-1] != '\n' {
		panic("expected LF")
	}

	rest := s[5 : len(s)-2]

	switch cmd := s[:5]; cmd {
	case CMD_SEND:
		return CMD_SEND, rest
	case CMD_RECV:
		return CMD_RECV, rest
	default:
		panic("invalid command " + cmd)
	}
}

func client() {
	conn, err := net.Dial("tcp", ADDR)
	if err != nil {
		panic(err)
	}

	for {
		send := "Test message"

		fmt.Fprintf(conn, "%s%s\n", CMD_SEND, send)
		fmt.Println("[CLIENT] Sent message")

		resp, err := bufio.NewReader(conn).ReadString('\n')
		if err != nil {
			panic(err)
		}
		fmt.Print("[CLIENT] Read message: ", resp)

		switch cmd, rest := whichCmd(resp); cmd {
		case CMD_RECV:
			if rest != send {
				panic("Mismatched command")
			}
		}
	}
}

var clients = []net.Conn{}

func serverBroadcast(cmd, s string) {
	for _, cl := range clients {
		fmt.Println("[SERVER] Broadcasting to", cl.LocalAddr())
		fmt.Fprintf(cl, "%s%s\n", cmd, s)
	}
}

func serverHelper(conn net.Conn) {
	for {
		msg, err := bufio.NewReader(conn).ReadString('\n')
		if err != nil {
			panic(err)
		}

		fmt.Println("[SERVER] Received message")

		switch cmd, rest := whichCmd(msg); cmd {
		case CMD_SEND:
			serverBroadcast(CMD_RECV, rest)
		default:
			panic("unexpected message " + msg)
		}
	}
}

func server() {
	ln, err := net.Listen("tcp", ADDR)
	if err != nil {
		panic(err)
	}
	fmt.Println("[SERVER] Started listening on address " + ADDR)

	for {
		conn, err := ln.Accept()
		if err != nil {
			panic(err)
		}
		fmt.Println("[SERVER] Received connection")
		clients = append(clients, conn)
		go serverHelper(conn)
	}
}

func main() {
	if len(os.Args) < 2 {
		panic("expected at least one argument <client|server> [args]")
	}

	switch arg := os.Args[1]; arg {
	case "client":
		client()
	case "server":
		server()
	default:
		panic("unexpected argument " + arg)
	}
}
