function(direction) {
    // 1. Look crap up
    var room 	    = Room.Find(this.room),
        label       = this.label,
        exits 	    = room.exits
        occu        = room.occupants,
        new_room    = Room.Find(exits[direction]);

    // 2. Bail out if there's no destination
    if (new_room === undefined) {
        this.notify("You can't go that way!");
        return;
    }
    this.room = new_room.id;
    
    // 3. Leave the old room and tell them about it
    room.departed(this.id);
    for (var i = 0; i < room.occupants.length; ++i) {
        var o = Entity.Find(room.occupants[i]);
        if (o.id != this.id) {
            o.notify(label + " has gone " + direction + ".");
        } else {
            this.notify("You move " + direction);
        }
    }
    
    // 5. Join the new room and tell them about it
    for (var i = 0; i < room.occupants.length; ++i) {
        var o = Entity.Find(room.occupants[i]);
        o.notify(label + " has entered the area.");
    }
    new_room.joined(this.id);

    // 6. Have a look 
    Commands.look && Commands.look.call(this, false);
}
