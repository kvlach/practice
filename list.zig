const std = @import("std");

const Item = struct {
    val: i32,
    prev: ?*Item = null,
    next: ?*Item = null,

    pub fn new(allocator: std.mem.Allocator, val: i32) !*Item {
        var it = try allocator.create(Item);
        it.val = val;
        return it;
    }
};

const List = struct {
    len: usize = 0,
    head: ?*Item = null,
    tail: ?*Item = null,
    allocator: std.mem.Allocator,

    pub fn new(allocator: std.mem.Allocator) !*List {
        var ls = try allocator.create(List);
        ls.allocator = allocator;
        ls.len = 0;
        return ls;
    }

    fn print(self: *List) void {
        if (self.len == 0) {
            return;
        }

        var it = self.head.?;

        for (0..self.len) |_| {
            std.debug.print("{d} ", .{it.val});
            it = it.next.?;
        }
        std.debug.print("\n", .{});
    }

    pub fn append(self: *List, val: i32) !void {
        if (self.len == 0) {
            self.head = try Item.new(self.allocator, val);
            self.tail = self.head;
        } else {
            var new_it = try Item.new(self.allocator, val);
            new_it.prev = self.tail;
            self.tail.?.next = new_it;
            self.tail = new_it;
        }

        self.len += 1;
    }
};

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var ls = try List.new(allocator);
    try ls.append(3);
    try ls.append(4);
    ls.print();
    return;
}
