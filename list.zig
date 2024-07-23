const std = @import("std");

const Item = struct {
    val: i32,
    prev: ?*Item,
    next: ?*Item,

    pub fn new(allocator: std.mem.Allocator, val: i32) !*Item {
        var it = try allocator.create(Item);
        it.val = val;
        it.prev = null;
        it.next = null;
        return it;
    }
};

const ListError = error{
    AtLeastOneItem,
};

const List = struct {
    len: usize,
    head: ?*Item,
    tail: ?*Item,

    pub fn new(allocator: std.mem.Allocator, len: usize) !*List {
        var ls = try allocator.create(List);

        if (len < 1) {
            return ListError.AtLeastOneItem;
        }

        const it = try Item.new(allocator, 0);
        ls.head = it;
        for (1..len) |_| {
            it.next = try Item.new(allocator, 0);
            var next = it.next.?;
            next.prev = it;
        }
        ls.tail = it;
        ls.len = len;

        return ls;
    }

    pub fn print(self: *List) void {
        var it = self.head.?;
        for (1..self.len) |_| {
            std.debug.print("{d} ", .{it.val});
            it = it.next.?;
        }
        std.debug.print("\n", .{});
    }

    // pub fn print(self: *List) void {
    //     var it = self.head;
    //     while (it) |item| {
    //         std.debug.print("{d} ", .{item.val});
    //         it = item.next;
    //     }
    //     std.debug.print("\n", .{});
    // }
};

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const ls = try List.new(arena.allocator(), 3);
    ls.print();
}
