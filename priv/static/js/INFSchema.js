(function() {
    "use strict";
    /* *************************************************************
     * Schema Initialization
     * Indicate path to any necessary schema and load them
     * Keep track of root URI for convenience in generating
     * absolute paths for instance registration.
     * ************************************************************/
    var infallible = "http://infallible.net/static/schema/infallible#";
    JSComposer.Schema.Load(infallible);

    /* *************************************************************
     * INFRoom
     * Form instance for Room objects
     *
     * Purpose:
     * Obscure system fields from auto-generated user interface:
     *      occupants
     * ************************************************************/
    function INFRoom(context, parent, schema, value) {
        JSComposer.ObjectInstance.apply(this, arguments);
        this.occupants_value = typeof value !== 'undefined' ? value.occupants : undefined;
    }

    // Inherit from JSComposer.ObjectInstance to adopt
    // object behaviour
    INFRoom.prototype = new JSComposer.ObjectInstance();
    INFRoom.prototype.constructor = INFRoom;

    INFRoom.prototype.AddEntry = function(type, key_desc, val_desc, key, value, options) {
        if (key === 'occupants') { return undefined; }
        JSComposer.ObjectInstance.prototype.AddEntry.apply(this, arguments);
    }

    INFRoom.prototype.GetValue = function() {
        var o = JSComposer.ObjectInstance.prototype.GetValue.call(this);
        o['occupants'] = this.occupants_value;
        return o;
    }

    /* *************************************************************
     * INFRoomOptions
     * Form instance for Room selector (enum of valid rooms)
     * ************************************************************/
    function INFRoomOptions(context, parent, schema, value) {
        // In this case, the options are preloaded. You could make
        // an AJAX request here instead.
        schema.enum = object_keys.room;
        JSComposer.SelectInstance.apply(this, arguments);
    }

    // Inherit from JSComposer.SelectInstance for select-box
    // enumeration behaviour.
    INFRoomOptions.prototype = new JSComposer.SelectInstance();
    INFRoomOptions.prototype.constructor = INFRoomOptions;

    /* *************************************************************
     * INFEntity
     * Form instance for Entity objects
     *
     * Purpose:
     * Obscure system fields from user auto-generated user
     * interface:
     *      handler
     *      active
     *      active_max
     *      modifiers
     * ************************************************************/
    function INFEntity(context, parent, schema, value) {
        Object.defineProperty(this, 'hidden_entries', {
            configurable:       true,
            enumerable:         true,
            value:              {}
        });

        this.super.apply(this, arguments);
    }

    INFEntity.HiddenProperties = [
        "handler",
        "active",
        "active_max",
        "modifiers"
    ];

    INFEntity.prototype = new JSComposer.ObjectInstance();
    INFEntity.prototype.constructor = INFEntity;
    INFEntity.prototype.super = JSComposer.ObjectInstance;

    INFEntity.prototype.AddEntry = function(type, key_desc, val_desc, key, value, options) {
        if (INFEntity.HiddenProperties.indexOf(key) < 0) {
            return this.super.prototype.AddEntry.apply(this, arguments);
        }

        this.hidden_entries[key] = this.CreateInstance(val_desc, value);
    };

    INFEntity.prototype.GetValue = function() {
        var o = this.super.prototype.GetValue.call(this);

        for (var k in this.hidden_entries) {
            if (!this.hidden_entries.hasOwnProperty(k)) continue;
            o[k] = this.hidden_entries[k].GetValue();
        }

        return o;
    }

    /* *************************************************************
     * INFEntityOptions
     * Form instance for Entity selector
     * ************************************************************/
    function INFEntityOptions(context, parent, schema, value) {
        // In this case, the options are preloaded. You could make
        // an AJAX request here instead.
        schema.enum = object_keys.entity;
        JSComposer.SelectInstance.apply(this, arguments);
    }

    INFEntityOptions.prototype = new JSComposer.SelectInstance();
    INFEntityOptions.prototype.constructor = INFEntityOptions;

    /* *************************************************************
     * INFRaceOptions
     * ************************************************************/
    function INFRaceOptions(context, parent, schema, value) {
        // In this case, the options are preloaded. You could make
        // an AJAX request here instead.
        schema.enum = object_keys.race;
        JSComposer.SelectInstance.apply(this, arguments);
    }

    INFRaceOptions.prototype = new JSComposer.SelectInstance();
    INFRaceOptions.prototype.constructor = INFRaceOptions;

    /* *************************************************************
     * INFSetting
     * ************************************************************/
    function INFSetting(context, parent, schema, value) {
        this.super.apply(this, arguments);
        if (typeof value === 'undefined') { value = {}; };
        console.log("!!!!!!!!!!!!!!!!!!! Value !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!");
        this.UpdateValueSchema(undefined, value);
    }

    INFSetting.ValueSchemas = {
        'room':         JSComposer.Schema.Ref("#/definitions/room_select", infallible),
        'entity':       JSComposer.Schema.Ref("#/definitions/entity_select", infallible),
        'race':         JSComposer.Schema.Ref("#/definitions/race_select", infallible),
        'command':      JSComposer.Schema.Ref("#/definitions/command_select", infallible),
        'integer':      {'type':'integer'},
        'number':       {'type':'number'},
        'string':       {'type':'string'}
    };

    INFSetting.prototype = new JSComposer.ObjectInstance();
    INFSetting.prototype.constructor = INFSetting;
    Object.defineProperty(INFSetting.prototype, 'super', {'configurable':false,'enumerable':false,'value':JSComposer.ObjectInstance});

    INFSetting.prototype.AddEntry = function(type, key_desc, val_desc, key, value, options) {
        if (key === 'value') {
            // We'll take control of the value from here
            val_desc = JSComposer.Schema.Ref("#/definitions/label");
        }

        this.super.prototype.AddEntry.call(this, type, key_desc, val_desc, key, value, options);
    }

    INFSetting.prototype.OnChange = function(type, source, entry) {
        var key = entry.GetKey();

        if (key === 'type') {
            this.UpdateValueSchema(entry.GetValue());
        }
    }

    INFSetting.prototype.UpdateValueSchema = function(type, value) {
        var entry = this.FindEntry('value'),
            schema = undefined;

        if (typeof type === 'undefined') {
            type = this.FindEntry('type').GetValue();
        }

        schema = INFSetting.ValueSchemas[type];
        if (schema === undefined) {
            schema = JSComposer.Schema.Ref("#/definitions/label");
        }
        entry.SetValueSchema(schema, value);
    }

    /* *************************************************************
     * INFCommandOptions
     * Selector instance for commands
     * ************************************************************/
    function INFCommandOptions(context, parent, schema, value) {
        // In this case, the options are preloaded. You could make
        // an AJAX request here instead.
        schema.enum = object_keys.command;
        this.super.call(this, context, parent, schema, value);
    }

    INFCommandOptions.prototype = new JSComposer.SelectInstance();
    INFCommandOptions.prototype.constructor = INFCommandOptions;
    Object.defineProperty(INFCommandOptions.prototype, 'super', {'configurable':false,'enumerable':false,'value':JSComposer.SelectInstance});

    /* *************************************************************
     * Register Instances
     * ************************************************************/
    // Object instances
    JSComposer.Instance.RegisterInstance(JSComposer.Schema.URI('#/definitions/room', infallible), INFRoom);
    JSComposer.Instance.RegisterInstance(JSComposer.Schema.URI('#/definitions/entity', infallible), INFEntity);
    JSComposer.Instance.RegisterInstance(JSComposer.Schema.URI('#/definitions/setting', infallible), INFSetting);

    // Selector instances
    JSComposer.Instance.RegisterInstance(JSComposer.Schema.URI('#/definitions/room_select', infallible), INFRoomOptions);
    JSComposer.Instance.RegisterInstance(JSComposer.Schema.URI('#/definitions/entity_select', infallible), INFEntityOptions);
    JSComposer.Instance.RegisterInstance(JSComposer.Schema.URI('#/definitions/race_select', infallible), INFRaceOptions);
    JSComposer.Instance.RegisterInstance(JSComposer.Schema.URI('#/definitions/command_select', infallible), INFCommandOptions);
}());
