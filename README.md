<center>
<img src="https://i.imgur.com/UruLe2H.png" alt="Drawing" width="100px"/>
<img src="https://i.imgur.com/DW2erAV.png" alt="Drawing" width="200px"/>
</center>

# Installation 

# Manually 

Clone this repository then run  'build.sh' which
 
- a) Compile and publish OpenMOLE on your local environment 

- b) Run gradle building process, which :
   - b.1) download dependencies on Gama p2 repository into `get-gama-p2/build/p2asmaven/p2/plugins/`, and remove the not needed `org.eclipse.osgi*` jar.
  - b.2) compile gama-openmole plugin jar using published dependencies during a) process. Jar is stored in `org.openmole.plugin.task.gama/build/lib/`

After that you could run your freshly openmole instance using command : 

`./openmole/openmole/bin/openmole/target/assemble/openmole --unoptimizedJS -p ../gama-gradle-plugin/get-gama-p2/build/p2asmaven/p2/p2/plugins/ --logger-level FINE`

In the OpenMole interface, open the extension windows and load the jar builded in `org.openmole.plugin.task.gama/build/lib/`

# Using Docker

Before any launch, the port, the uid/gid corresponding to your user in the provided `docker-compose.yml` file

Also set the `$VOL_PATH` environment variable in your shell, this location correspond to your workspace on OpenMOLE : 

```
export VOL_PATH = "my/volume/path"
```

The following command build OpenMOLE, and create a start-to-run docker environment. To use it:

```
docker-compose build 
docker-compose up
```

Open OpenMOLE at the corresponding port in your browser. If you access locally : https://localhost:xxx