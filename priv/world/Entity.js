function Entity(properties) {
    properties = properties || {};
    this.id         = Utils.uuid();
    this.base       = {};
    this.active_max = {};
    this.active     = {};
    this.room       = "default";

    for (var k in properties) {
        properties.hasOwnProperty(k) && (this[k] = properties[k]);
    }
}

Entity.Find = function(id) {
    if (typeof id != "string") {
        return;
    }

    System.log("Seeking entity: ", id);
    var entity = System.fetch("Entity", id);
    return entity;
}

Entity.Load = function(json) {
    System.log("Loading entity" + json);
    var o = JSON.parse(json);
    return new Entity(o);
}

Entity.LogoutDuration = 5000;

Entity.prototype.setHandler = function(handler) {
    if (handler === undefined) {
        System.log("Handler undefined!");
        return;
    }

    this.handler = handler;
}

Entity.prototype.notify = function(message) {
    System.notify(this.handler, message);
}

Entity.prototype.save = function() {
    System.save("Entity", this.id, JSON.stringify(this));
}

Entity.prototype.json = function() {
    System.log("JSONing!");
    return JSON.stringify(this);
}

Entity.prototype.update = function(json) {
    var o = JSON.parse(json);

    for (var k in o) {
        o.hasOwnProperty(k) && this.update_field(k, o[k]);
    }
}

Entity.prototype.update_field = function(k, v) {
    if (typeof(v) === "function") return;

    switch(k) {
        // Special update handling for certain fields here
        default:
            this[k] = v;
    }
}

Entity.prototype.tick = function() {
}

Entity.prototype.login = function() {
    if (this.logoutTimer !== undefined) {
        clearTimeout(this.logoutTimer);
        this.logoutTimer = undefined;
    } else {
        var o = Room.Find(this.room);
        o.joined(this.id);
    }

    Commands.look && Commands.look.call(this, false);
}

Entity.prototype.logout = function() {
    var self = this;
    System.log("Logout command executing: ", this.id);
    this.logoutTimer = setTimeout(function() { self.doLogout(); }, Entity.LogoutDuration);
    System.log("Timer set: ", this.logoutTimer);
}

Entity.prototype.doLogout = function() {
    System.log("doLogout command executing: ", this.id);
    var room = Room.Find(this.room),
        occ = room.occupants;

    room.departed(this.id);
    for (var i = 0; i < occ.length; ++i) {
        if (occ[i] == this.id) { continue; }
        var e = Entity.Find(occ[i]);
        e.notify(this.label + " fades away...");
    }
}

Entity.prototype.id                 = null;
Entity.prototype.label              = "Hugh Mann";
Entity.prototype.description        = "a nondescript being";
Entity.prototype.level              = 1;
Entity.prototype.experience         = 0;
Entity.prototype.handler            = null;
Entity.prototype.room               = null;
Entity.prototype.base               = null;
Entity.prototype.active_max         = null;
Entity.prototype.active             = null;
Entity.prototype.race               = null;
Entity.prototype.equipment          = {};
Entity.prototype.modifiers          = [];
Entity.prototype.inventory          = [];
