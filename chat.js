const chatbox = document.getElementById("chatbox");
const messages_area = document.getElementById("messages-area");
const ws = new WebSocket("ws://localhost:8000/");

const CMD_SEND = "SEND";
const CMD_RECV = "RECV";
const CMD_EROR = "EROR";
const CMD_JOIN = "JOIN";

ws.addEventListener("open", (event) => {
	console.log("[DEBUG] Opened WebSocket connection");

	const url_params = new URLSearchParams(window.location.search);
	const room = url_params.get("room");

	if (!room) {
		console.log("[ERROR] No room found, can't join");
		return
	}

	ws.send(`${CMD_JOIN} ${room}`);
	console.log("[DEBUG] Joined room:", room);
})

ws.addEventListener("close", (event) => {
	console.log("[DEBUG] Closed WebSocket connection");
})

ws.addEventListener("error", (event) => {
	console.log("[ERROR] WebSocket:", event.data);
})

ws.addEventListener("message", (event) => {
	console.log("[DEBUG] Received message:", event.data);

	const cmd = event.data.substring(0, 4);
	const rest = event.data.substring(5, event.data.length);

	switch (cmd) {
	case CMD_RECV:
		console.log("[DEBUG] RECV event:", rest);
		messages_area.innerHTML += "<div>"+rest+"</div>";
		break;
	case CMD_EROR:
		console.log("[ERROR] EROR event:", rest);
		break;
	default:
		console.log("[ERROR] Unknown command:", cmd);
	}
})

function joinRoom(room) {
	ws.send(`${CMD_JOIN} ${room}`);
}

function newMessage(msg) {
	console.log("[DEBUG] User attempting to send new message:", msg);
	ws.send(`${CMD_SEND} ${msg}`);
	chatbox.value = "";
}

