const chatbox = document.getElementById("chatbox");
const messages = document.getElementById("messages");
const sk = new WebSocket("ws://localhost:8000/");

sk.addEventListener("open", (event) => {
	console.log("[DEBUG] Opened WebSocket connection");
})

sk.addEventListener("close", (event) => {
	console.log("[DEBUG] Closed WebSocket connection");
})

sk.addEventListener("error", (event) => {
	console.log("[ERROR] WebSocket:", event);
})

sk.addEventListener("message", (event) => {
	console.log("[DEBUG] Received message from server:", event.data);

	const cmd = event.data.substring(0, 4);
	const rest = event.data.substring(5, event.data.length);

	switch (cmd) {
	case "RECV":
		messages.append(rest);
		break;
	default:
		console.log("[ERROR] Unknown command:", cmd);
	}
})

function joinRoom(name) {
	console.log("[DEBUG] User trying to join room:", name);
	sk.send(`JOIN ${name}`)
}

function newMessage(msg) {
	console.log("[DEBUG] User trying to send message:", msg);
	sk.send(`SEND ${msg}`)
	chatbox.value = "";
}
