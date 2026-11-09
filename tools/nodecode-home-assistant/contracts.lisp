;;;; contracts.lisp --- the four tools' arguments, one declaration each.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The one authority for a tool's shape: the JSON Schema the provider is
;;;; shown is projected from these, the value a body runs on is materialized
;;;; from them, and a call that does not satisfy one is refused before the
;;;; body sees it. They load before cell.lisp, whose bodies read the records.

(in-package #:nodecode-home-assistant)

(nlk:define-contract entity-list
  (:doc "List Home Assistant entities. On a large install a bare call answers thousands, so use the filters (domain, area, name, entity_ids) to keep the answer small and targeted. For a request about a room (\"the kitchen lights\"), filter by area, not name: a device belongs to a room by its Home Assistant area, and its name often does not mention the room. Judge what an entity is by the domain prefix of its entity_id (every light.* is a light), not by its name. An answer cut by a cap is marked truncated, with a note on how to narrow the query or get the rest.")
  ("domain" :string :default ""
   :doc "entity domain to filter by (light, switch, climate, sensor, binary_sensor, cover, fan, media_player...): the cheapest filter. Omit to list all entities")
  ("area" :string :default ""
   :doc "area/room name to filter by (living room, kitchen). Use this, not name, for any request about a room: an entity matches when its friendly name, its area attribute or its id carries the room")
  ("name" :string :default ""
   :doc "substring to match against entity friendly names and entity_ids (thermostat, temperature), for when the domain is unknown. Not for rooms: use area")
  ("entity_ids" :array :of :string
   :doc "exact entity ids to answer ([\"climate.office\",\"sensor.office_temp\"]). Returns just these, ignoring the domain/area/name filters")
  ("max" :integer :slot max-entities
   :doc "the most entities to answer for this call; overrides the operator's max_entities for this one call"))

(nlk:define-contract state-read
  (:doc "Get the detailed state of one Home Assistant entity: its state and every attribute (brightness, color, temperature setpoint, a sensor reading). last_changed, last_updated and last_triggered are Home Assistant's own UTC timestamps.")
  ("entity_id" :text :required t
   :doc "the entity to read (light.living_room, climate.thermostat, sensor.temperature)"))

(nlk:define-contract service-list
  (:doc "List the Home Assistant services (actions) available for device control: what can be done to each device type, and which fields each service takes. Use it to learn how to drive a device found with ha_list_entities, then call ha_call_service.")
  ("domain" :string :default ""
   :doc "the domain to list services for (light, climate, switch); omit for every domain"))

(nlk:define-contract service-call
  (:doc "Call a Home Assistant service to control a device. Use ha_list_services to learn a domain's services and their fields, and never guess an entity_id: call ha_list_entities first and use an exact id it answered. The service domain MUST match the entity's own domain: a light.* entity needs light.turn_on, a switch.* entity switch.turn_on, an input_boolean.* entity input_boolean.turn_on - the wrong domain affects nothing. The answer says which entities changed, or why none did (a missing entity, another domain, or a device that has not confirmed the change): follow what it says.")
  ("domain" :text :required t
   :doc "service domain (light, switch, climate, cover, media_player, fan, scene, script)")
  ("service" :text :required t
   :doc "service name (turn_on, turn_off, toggle, set_temperature, set_hvac_mode, open_cover, set_volume_level)")
  ("entity_id" :string :default ""
   :doc "the target entity id (light.living_room). Some services, such as scene.turn_on, may not need one")
  ("data" :object
   :doc "the service's own fields as a JSON object: {\"brightness\": 255, \"color_name\": \"blue\"} for light.turn_on, {\"temperature\": 22, \"hvac_mode\": \"heat\"} for climate.set_temperature, {\"volume_level\": 0.5} for media_player.volume_set"))
