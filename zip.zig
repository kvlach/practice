const std = @import("std");

const Node = struct {
	symbol: u8,
	probability: f64,
	// parent: ?*Node,
	child1: ?*Node,
	child2: ?*Node,

	fn init(symbol: u8, probability: f64) Node {
		return Node {
			.symbol = symbol,
			.probability = probability,
			// .parent = null,
			.child1 = null,
			.child2 = null,
		};
	}
};

const PriorityQueue = struct {
	queue: std.ArrayList(Node),

	fn init(allocator: std.mem.Allocator) PriorityQueue {
		return PriorityQueue {
			.queue = std.ArrayList(Node).init(allocator),
		};
	}

	fn deinit(self: *PriorityQueue) void {
		self.queue.deinit();
	}

	fn lowest_probability_index(self: *PriorityQueue) usize {
		std.debug.assert(self.queue.items.len != 0);

		var lowest_index: usize = 0;
		var lowest_probability: f64 = 10000000;
		for (0.., self.queue.items) |i, node| {
			if (lowest_probability > node.probability) {
				lowest_index = i;
				lowest_probability = node.probability;
			}
		}
		return lowest_index;
	}

	fn pop_lowest_probability(self: *PriorityQueue) Node {
		const i = self.lowest_probability_index();
		return self.queue.swapRemove(i);
	}

	fn append(self: *PriorityQueue, node: Node) !void {
		try self.queue.append(node);
	}

	fn root_node(self: *PriorityQueue) !Node {
		while (self.queue.items.len > 1) {
			var node1 = self.pop_lowest_probability();
			var node2 = self.pop_lowest_probability();
			var internal_node = Node.init(node1.symbol+node2.symbol, node1.probability+node2.probability);
			// node1.parent = &internal_node;
			// node2.parent = &internal_node;
			internal_node.child1 = &node1;
			internal_node.child2 = &node2;
			try self.append(internal_node);
		}
		return self.queue.items[0];
	}
};

pub fn main() !void {
	var gpa = std.heap.GeneralPurposeAllocator(.{}){};
	const allocator = gpa.allocator();

	var priority_queue = PriorityQueue.init(allocator);
	defer priority_queue.deinit();

	const node1 = Node.init(1, 15);
	const node2 = Node.init(2, 7);
	const node3 = Node.init(3, 6);
	const node4 = Node.init(4, 6);
	const node5 = Node.init(5, 5);

	try priority_queue.append(node1);
	try priority_queue.append(node2);
	try priority_queue.append(node3);
	try priority_queue.append(node4);
	try priority_queue.append(node5);

	const root = priority_queue.root_node();
	std.debug.print("{any}\n", .{root});

	const file_data = [_]u8{ 51, 52, 50, 54, 28, 124, 136, 11, 0 };
	const lit_code_0_143 = file_data[0];
	std.debug.print("{b}\n", .{lit_code_0_143});
}
