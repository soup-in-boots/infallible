function (notify) {
    System.log("Look command executing.");
    var room    = Room.Find(this.room),
        occ     = room.occupants,
        exits   = room.exits,
        desc    = room.description,
        res     = [];
        
    notify = notify === undefined ? true : notify;

    res.push(desc);
    
    if (occ.length > 1) {
        res.push("You also see:");
        for (var i = 0; i < occ.length; ++i) {
            if (notify && this.id == occ[i]) {
                this.notify("You look around.");
            } else if (occ[i] !== this.id) {
                var entity = Entity.Find(occ[i]);
                res.push([entity.label, "-", entity.description].join(" "));
                notify && entity.notify(this.label + " looked around.");
            } else {
                this.notify("You enter the room.");
            }
        }
    }
    if (exits.north || exits.south || exits.east || exits.west) {
        res.push("Obvious exits:");
        for (var dir in exits) {
            var o       = Room.Find(exits[dir]),
                capital = dir.charAt(0).toUpperCase() + dir.slice(1);
            
            res.push([capital, "-", o.label].join(" "));
        }
    }
    System.log("Sending response");
    this.notify(res.join("\r\n"));
}
