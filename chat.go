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
	if err == nil {
		fmt.Fprintf(os.Stderr, "[ERROR] %s\n", msg)
	} else {
		fmt.Fprintf(os.Stderr, "[ERROR] %s: %v\n", msg, err)
	}
}

func log_debug(a any) {
	fmt.Fprintf(os.Stderr, "[DEBUG] %v\n", a)
}

const (
	CMD_SEND = iota
	CMD_RECV
	CMD_EROR
	CMD_JOIN
)

var commandFmts = []string{"SEND", "RECV", "EROR", "JOIN"}

func commandParse(msg string) (int, string, error) {
	if !strings.HasSuffix(msg, "\n") {
		return 0, "", errors.New("expected LF at the end")
	}
	if len(msg) < 6 {
		return 0, "", errors.New("expected length of at least 6 (command + space + newline)")
	}
	if msg[4] != ' ' {
		return 0, "", errors.New("expected the 4th character to be a space")
	}

	rest := msg[5 : len(msg)-1]

	cmd := msg[:4]
	for index, fmt := range commandFmts {
		if cmd == fmt {
			return index, rest, nil
		}
	}
	return 0, "", errors.New("Couldn't match command " + cmd)
}

type Client struct {
	room string
	conn net.Conn
}

var clients []*Client

func NewClient(conn net.Conn) *Client {
	return &Client{conn: conn}
}

func reply(conn net.Conn, cmd int, msg string) error {
	_, err := fmt.Fprintf(conn, "%s %s\n", commandFmts[cmd], msg)
	return err
}

func broadcast(room string, cmd int, msg string) {
	for _, client := range clients {
		log_debug(client)
		if client.room == room {
			if err := reply(client.conn, cmd, msg); err != nil {
				log_error("failed to send "+commandFmts[cmd], err)
			}
		}
	}
}

func server(client *Client) {
	for {
		msg, err := bufio.NewReader(client.conn).ReadString('\n')
		if err != nil {
			log_error("failed to read from client", err)
			continue
		}

		cmd, rest, err := commandParse(msg)
		if err != nil {
			log_error(client.conn.LocalAddr().String(), err)
			reply(client.conn, CMD_EROR, err.Error())
			continue
		}

		log_debug(client)

		switch cmd {
		case CMD_SEND:
			if client.room == "" {
				reply(client.conn, CMD_EROR, "Must be in a room first")
			} else {
				broadcast(client.room, CMD_SEND, rest)
			}
		case CMD_JOIN:
			client.room = rest
			reply(client.conn, CMD_JOIN, rest)
		default:
			reply(client.conn, CMD_EROR, "Unexpected command "+commandFmts[cmd])
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
		clients = append(clients, NewClient(conn))
		go server(client)
	}
}
