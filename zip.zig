const std = @import("std");

const Node = struct {
	symbol: u8,
	probability: f64,
	parent: ?*Node,
	child_left: ?*Node,
	child_right: ?*Node,

	fn init(symbol: u8, probability: f64) Node {
		return Node{
			.symbol = symbol,
			.probability = probability,
			.parent = null,
			.child_left = null,
			.child_right = null,
		};
	}
};

const InternalNode = struct {
	probability: f64,
};

const PriorityQueue = struct {
	queue: std.ArrayList(Node),

	fn init(allocator: std.mem.Allocator) PriorityQueue {
		return PriorityQueue {
			.queue = std.ArrayList(Node).init(allocator),
		};
	}

	fn append(self: *PriorityQueue, node: Node) !void {
		try self.queue.append(node);
	}

	fn len(self: *PriorityQueue) usize {
		return self.queue.items.len;
	}

	fn lowest_probability(self: *PriorityQueue) usize {
		var lowest_index: usize = 0;
		var lowest_prob: f64 = 1000000000;

		for (0.., self.queue.items) |i, node| {
			if (lowest_prob > node.probability) {
				lowest_index = i;
				lowest_prob = node.probability;
			}
		}
		return lowest_index;
	}

	fn pop_lowest_probability(self: *PriorityQueue) Node {
		const i = self.lowest_probability();
		return self.queue.swapRemove(i);
	}
};

pub fn main() !void {
	var gpa = std.heap.GeneralPurposeAllocator(.{}){};
	const allocator = gpa.allocator();

	const node1 = Node.init(1, 15);
	const node2 = Node.init(2, 7);
	const node3 = Node.init(3, 6);
	const node4 = Node.init(4, 6);
	const node5 = Node.init(5, 5);

	var priority_queue = PriorityQueue.init(allocator);
	try priority_queue.append(node1);
	try priority_queue.append(node2);
	try priority_queue.append(node3);
	try priority_queue.append(node4);
	try priority_queue.append(node5);

	std.debug.print("{any}\n", .{priority_queue.queue.items});
	std.debug.print("{}\n", .{priority_queue.lowest_probability()});

	while (priority_queue.len() > 1) {
		var node_low1 = priority_queue.pop_lowest_probability();
		var node_low2 = priority_queue.pop_lowest_probability();
		var internal_node = Node.init(node_low1.symbol+node_low2.symbol, node_low1.probability+node_low2.probability);
		node_low1.parent = @constCast(&internal_node);
		node_low2.parent = @constCast(&internal_node);
		internal_node.child_left = @constCast(&node_low1);
		internal_node.child_right = @constCast(&node_low2);
		try priority_queue.append(internal_node);
	}

	const root_node = priority_queue.queue.items[0];
	std.debug.print("{any}\n", .{root_node});
}
