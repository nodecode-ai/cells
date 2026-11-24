# nodecode-ros

ROS Robot Control: drive and watch a ROS or ROS 2 robot over rosbridge, behind speed, geofence and a latching e-stop. A Nodecode rewrite of [arcabotai/hermes-ros](https://github.com/arcabotai/hermes-ros) by [arcabotai](https://github.com/arcabotai).

Nine tools and one `/ros` command: list the robot's topics, services and nodes; read a message; drive; publish; call a service; read the pose; take a camera snapshot; stop; and report the link and the safety state. Every write passes the interlock, and a refused call says which rule it broke. With the default `mock` backend none of it needs ROS: it is a 2D unicycle in a small arena with cones, service calls and a rendered camera, so the whole thing is demoable before a robot exists.

## Get it

Tell the agent "add the ros cell", or run `nodecode add nc://ros`.

## Set it up

Nothing is required for the mock robot, which is the default. For a real robot or a simulator, set `ros.backend` to `rosbridge` and give the machine and port that `rosbridge_server` listens on — the cell opens the websocket lazily, on the first tool call, and never at start:

| Setting | Default | What it is |
| --- | --- | --- |
| `backend` | `mock` | `mock` = the built-in 2D sim; `rosbridge` = a robot behind a rosbridge websocket |
| `host` | `localhost` | the rosbridge machine — a host name, not a URL |
| `port` | `9090` | the rosbridge websocket port |
| `timeout_seconds` | `8` | one service call's or connect's ceiling |
| `cmd_vel`, `odom`, `camera` | `/cmd_vel`, `/odom`, `/camera/image_raw` | the topics the tools use |
| `stamped` | off | wrap the velocity in `TwistStamped` (some bases want it) |
| `allow_topics`, `allow_services` | `/cmd_vel`, `/mock/*` | what may be written to, as globs |
| `geofence` | off | `x_min, x_max, y_min, y_max` in metres |
| `mock_arena`, `mock_cones` | `5,5`, `3.5,0.8` | the sim's half sizes, and its cones |
| `snapshots_kept` | `20` | how many camera frames this home keeps |

The safety numbers live in a `safety` object beside them, and stay at the plugin's defaults unless you set them: `max_linear_mps` 0.5, `max_angular_rps` 1.0, `max_duration_s` 5, `default_duration_s` 1, `over_limit_action` `block` (or `clamp`), `max_odom_age_s` 5, `margin_m` 0.25. See `config.example.jsonc`. A section that names `rosbridge` with a URL in `host` refuses at start, and `/cells` says which key is wrong.

## Use it

| Tool | What it does |
| --- | --- |
| `ros_list` | topics with their types, services and nodes, filtered |
| `ros_echo` | one message from a topic, big arrays summarised |
| `ros_drive` | publish a velocity for a bounded duration, then stop, and answer the pose before and after |
| `ros_publish` | a message to an allowlisted topic, optionally repeated; a `Twist` is checked as motion |
| `ros_call_service` | a service on the allowlist |
| `ros_get_pose` | x, y, yaw, velocities and the collision flag from odometry |
| `ros_camera_snapshot` | one frame, saved as a PNG or JPEG under this home, with the `MEDIA:` line that shows it |
| `ros_stop` | a zero-velocity burst; `estop=true` also latches the e-stop |
| `ros_status` | backend, limits, geofence, allowlists, the latch, the blocked calls |

The interlock refuses a speed or duration over the limit (or clamps it, in `clamp` mode), a projected path that leaves the geofence, a stale or missing odometry, a topic or service outside the allowlist, and any motion while the e-stop is latched. Every refusal names its rule, and the last 200 are kept for `/ros log`. Only a human clears a latch: the agent has no tool for it.

For the operator, `/ros status`, `/ros estop [REASON]`, `/ros clear`, `/ros log [N]`, `/ros snapshot` and `/ros stop` reach the same link; `(help :ros)` is the primer, and the verbs are callable from eval too: `(ros:pose)`, `(ros:drive 0.2 0 1)`.

## What is in it

| File | What it is |
| --- | --- |
| `robot.lisp` | the two backends: the mock sim and the rosbridge websocket |
| `cell.lisp` | the nine verbs, the interlock, the contracts and the `/ros` command |
| `package.lisp` | the package, its verbs and the small words they share |
| `config.example.jsonc` | the `ros` section, commented |
| `test/` | the offline tests: 14, with only the socket stubbed |

## What the rewrite does not do

The plugin's Hermes dashboard tab and its read-only web API, its separate plugin data directory, and its bundled skill prompt: a cell has `/ros` and the manual instead, and keeps the latch in its own file under this home. See `NOTICE`.

## License

MIT, the original's: see `LICENSE` and `NOTICE`.
