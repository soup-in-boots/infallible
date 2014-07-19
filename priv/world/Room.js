function Room(properties) {
    properties = properties || {};

    for (var k in properties) {
        if (properties.hasOwnProperty(k)) {
            this[k] = properties[k];
        }
    }
    this.occupants = [];
}

Room.Load = function(json) {
    System.log("Loading Room: ", json);
    var o = JSON.parse(json);
    return new Room(o);
}

Room.Find = function(id) {
    if (typeof id != "string") {
        return undefined;
    }
    var room = System.fetch("Room", id);
    if (room === undefined) {
        var d = System.getSetting("default_room");
        room = System.fetch("Room", d);
    }

    return room;
}

Room.prototype.joined = function(id) {
    if (this.occupants.indexOf(id) >= 0) { return; }
    this.occupants.push(id);
}

Room.prototype.departed = function(id) {
    System.log("Departed: ", id);
    var pos = this.occupants.indexOf(id);
    while (pos >= 0) {
        System.log(id + " departed [" + pos + "]");
        this.occupants.splice(pos, 1);
        System.log("New occupant length: ", this.occupants.length);
        pos = this.occupants.indexOf(id);
    }
}

Room.prototype.json = function() {
    return JSON.stringify(this);
}

Room.prototype.update = function(json) {
    var o = JSON.parse(json);

    for (var k in o) {
        o.hasOwnProperty(k) && this.update_field(k, o[k]);
    }
}

Room.prototype.update_field = function(k, v) {
    if (typeof(v) === "function") return;

    switch(k) {
        // Special update handling for certain fields here
        default:
            this[k] = v;
    }
}

Room.prototype.tick = function() {
    System.log("Occupants: ", this.occupants.join(" "));
}

Room.prototype.id           = null;
Room.prototype.label        = "Just a Room";
Room.prototype.description  = "You see four walls, a ceiling, and a floor.";
Room.prototype.exits        = {};
Room.prototype.occupants    = [];
