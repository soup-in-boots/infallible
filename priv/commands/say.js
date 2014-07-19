function() {
    var message     = [],
        label       = this.label,
        room        = Room.Find(this.room),
        occ         = room.occupants;
        
    for (var i = 0; i < arguments.length; ++i) {
        message.push(arguments[i]);
    }
    message = message.join(" ");
        
    for (var i = 0; i < occ.length; ++i) {
        if (this.id == occ[i]) {
            this.notify("You say \"" + message + "\"");
        } else {
            var e = Entity.Find(occ[i]);
            e.notify(label + " says \"" + message + "\"");
        }
    }
}
