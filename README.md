Repository of the master thesis ["The Hera framework for fault-tolerant sensor fusion on an Internet of Things network with application to inertial navigation and tracking"](thesis.pdf) by Sébastien Kalbusch and Vincent Verpoten.
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

Additionally, if you want to reproduce some of our experiments you might need to purchase a toy train (https://www.lgb.com/products/details/article/90463/).

## Required software
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

If you want to have more in-depth information about configuration files here are a few useful links:
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

After that, you can plug the sd cards in their respective GRiSP boards.
Then, you should plug the Pmod sensors:

| Pmod      | Slot  |
| :---:     | :---: |
| NAV       | SPI1  |
| MAXSONAR | UART  |

Finally, you can connect the board to the battery.
During the boot phase you should see one green LED.
After ~5 min you should see two red LEDs or two green LEDs (see later).
In case of problems we advise you to connect the GRiSP-base by serial https://github.com/grisp/grisp/wiki/Connecting-over-Serial.
You can use `make screen` once the cable is plugged in.

In parallel you can start the application on your computer.
You can either have a clean start with:
```bash
make local_release && make run_local
```
or start in developement mode with:
```bash
make shell
```
We advise you to start in developement mode.
Note that if you use the release the measures folder will be created in the release root directory.
After a few seconds a message will be displayed telling you that the application is booted.
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
The former is used for the experiments with the train while the latter is used for the 6 DOF IMU.
The 6 DOF IMU requires 3 sonars and each of them must be placed on a different axis (x,y,z).
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
The former is used for the experiments with the train while the latter is used for the 6 DOF IMU.
Once you press enter, instructions will appear on your screen.
Follow these instructions.

### Launching the measurements
You can launch the whole cluster from any node with:
```erlang
sensor_fusion:launch_all().
```
Alternatively you can launch a single node from the remote shell with:
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
We have created 4:
- train tracking
- sonar range
- 3d orientation
- 3d position

Then, click on the button "data.csv" and select the csv file you want to read.
Each csv file produced by the erlang application starts with the name of the measure.
Here is the correspondence between the view selection and the measure names:

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

### Creating a new measure process
You can easily add a new measure by creating a new `hera_measure` behaviour module.
You only need to provide two functions: `init/1` and `measure/1`.

The `init/1` callback takes as argument any Erlang term and must return a tuple of the following type:
```erlang
{ok, State :: term(), Spec :: measure_spec()}.
```

Where `Spec` is a map specified by:
```erlang
-type measure_spec() :: #{
	name := atom(), % measure id
	iter := pos_integer() | infinity, % number of measures to perform
	sync => boolean(), % must the measure must be synchronized? (default: false)
	timeout => timeout() % min delay between two measures (default: 1)
}.
```

The argument of `measure/1` can be any Erlang term.
You can use it for all sorts of things like a state variable or calibration data.
This callback must return a tuple of the following type:
```erlang
{ok, Values, NewState} | {undefined, NewState} when
Values :: [number(), ...],  
NewState :: term().
```

To start the measure you should call:
```erlang
hera:start_measure(Module, Args).
```

Where `Module` is your behaviour module and `Args` will be passed as argument to
`init/1`.
If the `Args` passed to `hera:start_measure/2` must be persistent you can store it in the system with:
```erlang
sensor_fusion:update_table({{Key, node()}, Args}).
```

And later retrieve it with:
```erlang
ets:lookup_element(args, {Key, node()}, 2).
```

For more information, we advise you to look at some examples in this application.
You should also look in [sensor_fusion.erl](src/sensor_fusion.erl) to see how we launch the system and manage the persistent data.

### Adding a new sensor
The first thing to do, is to enable the driver of the Pmod sensor with:
```erlang
grisp:add_device(Slot, Driver).
```

Where `Slot` is the lower case version of the slot name printed on the board itself and `Driver` is the name of the driver module (All the drivers can be found at https://github.com/grisp/grisp/tree/master/src).

Then, you can create a new measure process.
To read the sensor, you simply need to call the sensor driver *get* function in the `measure/1` callback of your `hera_measure` behaviour module.
For instance:
```erlang
measure(Calibration) ->
	RawData = pmod_nav:read(acc, [out_x_xl]),
	% ... do something with the RawData and the Calibration
	{ok, CorrectedData, Calibration}.
```

In this example, we receive a calibration as argument.
Here are the steps to follow if you need a calibration:

1. Create and export a calibration function in your module.
2. Before you start the measure, call your calibration function and store the output with `sensor_fusion:update_table/1`.
4. Your `init/1` callback should forward the data in the `State` variable.


If you need to, you can also update the state with the `NewState` variable in the output of your `measure/1` callback.
        
### Adding a new sensor fusion model
In Hera, we perform sensor fusion via `hera_measure` modules.
Each module has an `init/1` and a `measure/1` callback.
In the `measure/1` callback, we can fetch the data from the local data store with `hera_data:get/1` and `hera_data:get/2`.
You can discard old data with a simple timestamp filter.
Then, you can write the parameters required for your sensor fusion model using list comprehensions and the fetched data.
In our case, we use a Kalman filter and so, we write all the "variadic" matrices needed.
Finally, we give these parameters as argument to the Kalman function, included as library in Hera, and recover the result.

Here is a short example where we fetch new (not used in the previous computation and arrived since at most 500 [ms]) sonar data, create the matrices and call the Kalman filter.
Then, we serialize the matrix containing the state vector and we update the state of the model.
As you can see, we update a timestamp to avoid reusing data multiple times.

```erlang
measure(State = {T0, X0, P0}) ->
	DataSonars = hera_data:get(sonar),
	T1 = hera:timestamp(),
	Sonars = [{Node,Data} || {Node,Seqnum,Ts,Data} <- DataSonars, T0 < Ts, T1-Ts < 500],
	Matrix1 = ...
	Matrix2 = ... 
	{X1, P1} = kalman:kf(State, Matrix1, Matrix2, ...), 
	NewState = {T1, X1, P1},
	Values = lists:append(X1),
	{ok, Values, NewState}.
```

Our Kalman library offers two Kalman filters: the linear Kalman filter without control input and the extended Kalman filter without control input.
We also separately implement the predict and update phase for the linear Kalman filter, allowing to use them independently if needed.
A matrix library is also included in Hera and offers common matrix operations in the `mat` module. 

For more information, we advise you to look at some examples in this application.
            
### Adding new libraries to Hera (Kalman filters, matrix operations, ...)
The Hera framework comes with 2 implementations of [Kalman filters](https://github.com/sebkm/hera/blob/main/src/kalman.erl): the linear Kalman filter without control input and the extended Kalman filter without control input.
For some applications, new Kalman filters or even different data fusion algorithm can be added to Hera in a straightforward manner.
Hera also provide a [matrix library](https://github.com/sebkm/hera/blob/main/src/mat.erl) that could be extended with new operations.
The only constraint is that the algorithms must be implemented by pure functions.
The purpose is simply to provide a reusable toolbox for the `hera_measure` modules where all the states and side effects should be embedded.


### Updating the code
When you make a change to the code, you can compile it with `rebar3 compile` and even have additional type checks (recommended) with:
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
This will make the update permanent (even after a reboot).
The Application must already exist on each GRiSP-base, but the module may be new.
However, if it is new, the module will not be loaded on start-up.

Note that if you update hera your changes will only be applied after a reboot of hera.
You can force it with `sensor_fusion:stop_all/0`.

If you made lots of changes you should consider deploying the application again.

### Adding a new view in LiveView
Creating your own view requires only 3 steps:
1. Create a figure initialization callback (e.g. [initTrain.m](liveView/initTrain.m)) that should return handles to your axes.
2. Create a figure update callback (e.g. [updateTrain.m](liveView/updateTrain.m)) that receives the last measure as well as the handles.
	A data global variable can be used to store data in a more permanent way.
	The data will be erased upon leaving the view.
3. Adding a new entry in the view selection menu as well as your callbacks in [liveView.m](liveView/liveView.m):
```matlab
initView = {@initTrain, @initSonar};
updateView = {@updateTrain, @updateSonar};
views = {"train tracking", "sonar range"};
```
Currently, liveView only enables you to read from one file, but you can have multiple instances of liveView at the same time.
This way, you can visualize data from multiples sources.
If you need to visualize the data before the measurement is even started on the Erlang application, you can create the csv file yourself and start liveView.
As soon as it will be written, the view will be updated.
