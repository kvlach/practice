package main

import (
	"bufio"
	"errors"
	"fmt"
	"net"
)

type client struct {
	conn net.Conn
	room string
}

var clients []*client

func NewClient(conn net.Conn) *client {
	return &client{conn: conn}
}

func findClient(conn net.Conn) *client {
	for _, client := range clients {
		if client.conn == conn {
			return client
		}
	}
	panic("client not found")
}

const CMD_SEND = "SEND "
const CMD_RECV = "RECV "
const CMD_EROR = "EROR "
const CMD_JOIN = "JOIN "

func parseCmd(msg string) (string, string, error) {
	if len(msg) < 6 {
		return "", "", errors.New("expected at least 6 characters (command + space + newline)")
	}
	if msg[len(msg)-1] != '\n' {
		return "", "", errors.New("expected newline at the end")
	}
	if msg[4] != ' ' {
		return "", "", errors.New("expected space between command")
	}

	rest := msg[5 : len(msg)-1]

	switch cmd := msg[:5]; cmd {
	case CMD_SEND, CMD_RECV, CMD_EROR, CMD_JOIN:
		return cmd, rest, nil
	default:
		return "", "", errors.New("unknown command " + cmd)
	}
}

func send(conn net.Conn, cmd, rest string) {
	fmt.Fprintf(conn, "%s%s\n", cmd, rest)
}

func broadcast(cmd, rest, room string) {
	for _, client := range clients {
		if client.room == room {
			send(client.conn, cmd, rest)
		}
	}
}

func server(conn net.Conn) {
	for {
		msg, err := bufio.NewReader(conn).ReadString('\n')
		if err != nil {
			fmt.Printf("[DEBUG] Client error: %v\n", err)
			return
		}

		fmt.Println("[DEBUG] Received message")

		cmd, rest, err := parseCmd(msg)
		if err != nil {
			send(conn, CMD_EROR, err.Error())
			continue
		}

		client := findClient(conn)

		switch cmd {
		case CMD_SEND:
			if client.room == "" {
				send(conn, CMD_EROR, "must join a room first")
			} else {
				broadcast(CMD_SEND, rest, client.room)
			}
		case CMD_JOIN:
			client.room = rest
		default:
			send(conn, CMD_EROR, "Unknown command "+cmd)
		}
	}
}

func main() {
	ln, err := net.Listen("tcp", "localhost:1234")
	if err != nil {
		panic(err)
	}

	for {
		conn, err := ln.Accept()
		if err != nil {
			panic(err)
		}
		clients = append(clients, NewClient(conn))
		go server(conn)
	}
}
