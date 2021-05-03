Repository of the master thesis "Sensor fusion at the extreme edge of an internet of things network" by Sébastien Kalbusch and Vincent Verpoten.
This repository is based on the work of a previous master thesis from Julien Bastin and Guillaume Neirinckx.
You can find their repository here: https://github.com/bastinjul/sensor_fusion

# User manual

## Required hardware
To use our system you need:
- 1 computer
- 1 wifi access point (a smartphone is enough)
- 4 GRiSP-base (with sd card)
- 1 Pmod NAV
- at least 3 Pmod MAXSONAR
- 4 batteries (with micro-usb connector)

You can find all GRiSP related hardware at https://www.grisp.org/shop/.

Additionally, if you want to reproduce some of our experiments you might need https://www.lgb.com/products/details/article/90463/.

## Required softwares
To use our system you need to install on your computer:
- Erlang/OTP 22.0
- rebar3 3.13.0
- rebar3_hex 6.9.6
- rebar3_grisp 1.3.0
- GNU Octave (only for the visualization tool)

It is very important to install the specified versions because the most recent versions are not compatible.
We advise you to work on GNU/Linux and to follow this tutorial https://github.com/grisp/grisp/wiki in case of problems.
First, you can install [Erlang/OTP](https://www.erlang.org/downloads/22.0).
Then, you can install [rebar3](https://github.com/erlang/rebar3/releases/tag/3.13.0) and follow this tutorial https://github.com/erlang/rebar3/#getting-started.
When this is done, you should specify the plugins in ~/.config/rebar3/rebar.config:

```erlang
{plugins, [
	{rebar3_hex, "6.9.6"},
    {rebar3_grisp, "1.3.0"}
]}.
```

and run the following command to update the plugins and verify if they are correctly installed:
```bash
rebar3 update && rebar3 plugins list
```

## Configuration files
### Network configuration
To connect the GRiSP boards via existing wifi network, you first need put the information relative to your network in [wpa_supplicant.conf](grisp/grisp_base/files/wpa_supplicant.conf).
Then, you must indicate the IP addresses and hostnames in [erl_inetrc](grisp/grisp_base/files/erl_inetrc).
For example:

```erlang
{host, {192,168,43,217}, ["sebastien"]}. % computer node
{host, {192,168,43,12}, ["sonar_1"]}.
{host, {192,168,43,142}, ["sonar_2"]}.
{host, {192,168,43,245}, ["sonar_3"]}.
{host, {192,168,43,90}, ["nav_1"]}.
{host, {192,168,43,206}, ["nav_2"]}.
```

To find the IP of a board, you can perform a network scan or follow the tutorial from grisp.
Finally, you should write this information on your computer in [/etc/hosts]().
The format is not the same.
Here is an example:
```
127.0.1.1  sebastien
192.168.43.12 sonar_1
192.168.43.142 sonar_2
192.168.43.245 sonar_3
192.168.43.90 nav_1
192.168.43.206 nav_2

```
If you wish to use our system as is then you must follow the same hostnames as shown in the example (More on that later).

### Other configurations
[computer.config.src](config/computer.config.src) is used for the computer node.
The log_data variable must be set to true if you wish to receive the data collected on the network.
```erlang
{hera, [
    {log_data, true}
]}
```
The data will be written in **csv** format in [measures/](./measures/).
The file names follow a specific nomenclature: *measureName_sensor_fusion@hostname.csv*.

[sys.config](config/sys.config) is used for the GRiSP nodes.
An error logger is setup to generate a report in LOGS/ERROR.1 on the sd card.
If you suspect something has gone wrong with the system you should look there, but be aware that the information might be outdated as these files are never removed.

[vm.args](config/vm.args) must contain:
```
## Name of the node
-sname sensor_fusion

## Cookie for distributed erlang
-setcookie MyCookie
```

Finally, [rebar.config](rebar.config) contains all the information to build and deploy the system.
The only part you should modify is the path to the sd card on which you want to deploy the system.
In this example, the sd card is names "GRISP".
``` erlang
{deploy , [
	{pre_script, "rm -rf /media/sebastien/GRISP/*"},
	{destination, "/media/sebastien/GRISP"},
	{post_script, "umount /media/sebastien/GRISP"}
]}
```

If you want to have more indepth information about configuration files here are a few useful links:
- https://github.com/grisp/grisp/wiki
- https://github.com/grisp/rebar3_grisp
- https://github.com/erlang/rebar3
- http://erlang.org/doc/man/config.html
- http://erlang.org/documentation/doc-5.9/doc/design_principles/distributed_applications.html


## Deployment
The first thing to do is to format each sd card as **fat32**.
We also suggest to name it "GRISP".
The easiest way to achieve that is to use a partitioning tool like "KDE Partition Manager" or similar.
You only need to this once.

To ease the process we created a make file that we use as a command shortcut.
You can deploy the software on each sd card with the `make deploy-hostname` command where hostname is the name of the GRiSP board.
This will take some time.
For example: 
```bash
make deploy-nav_2
```

After that, you can plug the sd cards in their respective GRiSP boards and connect them to the battery.
During the boot phase you should see one green LED.
After ~5 min you should see two red LEDs or two green LEDs (see later).
In case of problems we advise you to connect the GRiSP-base by serial https://github.com/grisp/grisp/wiki/Connecting-over-Serial.
You can use `make screen` once the cable is plugged in.

In parallel you can start the application on your computer.
You can either have a clean start with:
```bash
make local_release && make run_local
```
or start in developpement mode with:
```bash
make shell
```
We advise you to start in developpement mode.
Note that if you use the release the measures folder will be created in the release root directory.
After a few seconds a message will be displyed telling you that the application is booted.
You might need to press enter to get the shell prompt.

## Launching the system

### Calibration
Certain sensors require a calibration in order to be used.
If the information is not present on the system, you should see two red LEDs.
On the other hand, if the information is present, you should see two green LEDs.
Note that in case of restart, as long as there is at least one node staying alive, the information will remain available.

The calibration routine depends on the node hostname as well as the type of measurements that you wish to perform.
The first step we suggest is to open a remote shell.
If you know what you are doing you can also perform the calibrations with the `rpc` module.
When the calibration is done you can close the remote shell.

#### Calibration of a sonar node
First, you should open a remote shell to the node.
For instance:
```bash
make remote-sonar_1
```
Then you must call:
```erlang
sensor_fusion:set_args(sonar, Arg1, Arg2, Arg3).
```
Where `Arg1` is the maximal range that can be measured by the sonar while `Arg2` and `Arg3` are either the x and y coordinate of the sonar or the distance to the origin (0,0,0) and the direction (-1 or +1) of the sonar with respect to the axis it is aligned with.
The former is used for the experiments with the train while the latter is used for the 6DOF IMU.
The 6DOF IMU requires 3 sonars and each of them must be placed on a different axis (x,y,z).
If you need to, you can use `sonar:range/0` to see what the sonar is measuring. 

#### Calibration of a nav node
First, you should open a remote shell to the node.
For instance:
```bash
make remote-nav_1
```
Then you must call:
```erlang
sensor_fusion:set_args(Nav).
```
Where `Nav` is either `nav` or `nav3`.
The former is used for the experiments with the train while the latter is used for the 6DOF IMU.
Once you press enter, instructions will appear on your screen.
Follow these instructions.

### Launching the measurements
You can launch the whole cluster from any node with:
```erlang
sensor_fusion:launch_all().
```
Alternaltively you can launch a single node from the remote shell with:
```erlang
sensor_fusion:launch().
```
If the LEDs switch to green, you can consider the system to be launched.
If they switch to red (or remain red) it means the calibration data was not found.

If you wish to stop the measurements on the whole cluster you can use:
```erlang
sensor_fusion:stop_all().
```

### LiveView
You can visualize the measurements in soft real time with the LiveView tool.
To start it just do:
```bash
make liveView
```
This will open a small GUI.
First, select the view you want.
We have created 3:
- train tracking
- sonar range
- 3d orientation
- 3d position

Then, click on the button "data.csv" and select the csv file you want to read.
Each csv file produced by the erlang application starts with the name of the measure.
Here is the correspondance between the view selection and the measure names:

| view name      | measure name   |
| :---:          | :---:          |
| train tracking | e5,e6,e7,e8,e9 |
| sonar range    | sonar          |
| 3d orientation | e11            |
| 3d position    | e10            |

Then, click on the "start" button.
You should see a colored square on the top right of the screen.
If the square is green it means data is being received in soft real time, but if it is red then nothing is being received.

The tool also provides a "replay" mode that you can use to review the measures with a "real time" feel.

## Development
Our application is made to be extended.
The dynamic measurements are handled by the framework "hera" and the actual measurements as well as the calibration storage are handled by the application "sensor_fusion".
The framework will not be explained in this document.

### Adding a new measure
You can easily add a new measure by creating a new hera_measure behaviour module.
You only need to provide two functions: `init/1` and `measure/1`.
To start the measure you should call:
```erlang
hera:start_measure(Module, Args).
```
Where `Module` is your behaviour module and `Args` will be passed as argument to
`init/1`.
It can be whatever you want.
The argument of `measure/1` is a state variable.
You can use it for all sorts of things.
We often use it as calibration data.
For more information we advise you to look at some examples in this application.
You can find the exact specification in [hera_measure.erl](https://github.com/sebkm/hera/blob/main/src/hera_measure.erl).

If the `Args` passed to `hera:start_measure/2` must be persistant you can store it in the system with:
```erlang
sensor_fusion:update_table({{Key, node()}, Args}).
```
You can look in [sensor_fusion.erl](src/sensor_fusion.erl) for usage examples.

### Updating the code
When you make a change to the code, you can compile it with `rebar3 compile` and even have additional type checks (recommanded) with:
```bash
rebar3 dialyzer
```
In case you made changes to dependencies for example by placing it in [_checkouts/](_checkouts/), you should run `make clean` and remove build files in the dependency folder before compiling.
If you change something in [kalman.erl](https://github.com/sebkm/hera/blob/main/src/kalman.erl) or in [mat.erl](https://github.com/sebkm/hera/blob/main/src/mat.erl) you are encouraged to run the tests:
```bash
make test
```

If you are running the application with `make shell`, you can update the code live (without leaving the application) on all the nodes in the cluster with:
```erlang
sensor_fusion:update_code(Application, Module).
```
Where `Application` and `Module` are the application and module you wish to update.
This will make the update permanant (even after a reboot).
The Application must already exist on each GRiSP-base, but the module may be new.
However, if it is new, the module will not be loaded on start-up.

Note that if you update hera your changes will only be applied after a reboot of hera.
You can force it with `sensor_fusion:stop_all/0.`

If you made lots of changes you should consider deploying the application again.

### Adding a new view in LiveView
Creating your own view requires only 3 steps:
1. Create a figure initialization callback (e.g. [initTrain.m](liveView/initTrain.m)) that should return handles to your axes.
2. Create a figure update callback (e.g. [updateTrain.m](liveView/updateTrain.m)) that receives the last measure aswell as the handles.
	A data global variable can be used to store data in a more permanent way.
	The data will be erased uppon leaving the view.
3. Adding a new entry in the view selection menu as well as your callbacks in [liveView.m](liveView/liveView.m):
```matlab
initView = {@initTrain, @initSonarx};
updateView = {@updateTrain, @updateSonar};
views = {"train tracking", "sonar range"};
```
Currently, liveView only enables you to read from one file, but you can have multiple instances of liveView at the same time.
This way, you can visualize data from multiples sources.
If you need to visualize the data before the measurement is even started on the Erlang application, you can create the csv file yourself and start liveView.
As soon as it will be writen, the view will be updated.
