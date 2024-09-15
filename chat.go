package main

import (
	"bufio"
	"errors"
	"fmt"
	"net"
	"os"
	"strings"
)

func log_error(msg string, err error) {
	fmt.Fprintf(os.Stderr, "[ERROR] %s: %v\n", msg, err)
}

func log_debug(a any) {
	fmt.Fprintf(os.Stderr, "[DEBUG] %v\n", a)
}

type Client struct {
	room string
	conn net.Conn
}

var clients []*Client

func NewClient(conn net.Conn) *Client {
	return &Client{conn: conn}
}

func clientPop(client *Client) {
	for i, c := range clients {
		if c == client {
			clients[i] = clients[len(clients)-1]
			clients = clients[:len(clients)-1]
			return
		}
	}
}

const (
	CMD_SEND = iota
	CMD_JOIN
	CMD_EROR
	CMD_QUIT
)

var commandFmts = []string{"SEND", "JOIN", "EROR", "QUIT"}

func commandParse(s string) (int, string, error) {
	if !strings.HasSuffix(s, "\n") {
		return 0, "", errors.New("expected newline as a last character")
	}
	if len(s) < 6 {
		return 0, "", errors.New("expected at least 6 characters (command + space + newline)")
	}
	if s[4] != ' ' {
		return 0, "", errors.New("expected the 5th character to be a space")
	}

	rest := s[5 : len(s)-1]
	cmd := s[:4]

	for i, fmt := range commandFmts {
		if cmd == fmt {
			return i, rest, nil
		}
	}
	return 0, "", errors.New("unknown command: " + cmd)
}

func commandRender(cmd int) string {
	return commandFmts[cmd]
}

func send(conn net.Conn, cmd int, text string) {
	fmt.Fprintf(conn, "%s %s\n", commandRender(cmd), text)
}

func broadcast(room string, cmd int, text string) {
	for _, client := range clients {
		log_debug(client)
		if client.room == room {
			send(client.conn, cmd, text)
		}
	}
}

func server(client *Client) {
	for {
		msg, err := bufio.NewReader(client.conn).ReadString('\n')
		if err != nil {
			log_error("failed to read client message", err)
			continue
		}

		log_debug("received message: " + msg)

		cmd, rest, err := commandParse(msg)
		if err != nil {
			send(client.conn, CMD_EROR, err.Error())
			continue
		}

		switch cmd {
		case CMD_SEND:
			if client.room == "" {
				send(client.conn, CMD_EROR, "must be in a room")
			} else {
				broadcast(client.room, cmd, rest)
			}
		case CMD_JOIN:
			client.room = rest
			send(client.conn, cmd, rest)
		case CMD_QUIT:
			clientPop(client)
			return
		default:
			panic("unreachable")
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
			log_error("failed to accept connection", err)
			continue
		}
		client := NewClient(conn)
		clients = append(clients, client)
		go server(client)
	}
}
