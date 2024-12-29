### Issues
Slice size: 
- 100 OK but there are too much slices files, hard to manage
- 500 
    - "java.lang.OutOfMemoryError: Java heap space" at joern_create() "importCpg" line (solved see bottom)
        - increase max heap size, see bottom section
        - increase process timeout in cpg_generator.py in joern_create()

In CPG_generator
- Generate json with joern_create: some Cpg.bin has methods that freeze the generation when parsing the PDG edges
    - this happens on few samples (4 for me) 
    - my solution is to skip these methods parsing with graph-for-funcs_DEBUG.sc manually with joern (parse, see debug prints and then change line 77 and re-parse)
   
Training
- the model predict always 0, due to vanishing gradients
   - Remove the log operation during forward (and also th.clamp but it is not present in original repo)
   - Use leaky_relu instead of relu in the conv layers (seems to have better performance)

### Dataset preprocess
Look at `select` function in `run.py`
### Joern version v2.0.291

[Joern version v2.0.291](https://github.com/joernio/joern/releases/tag/v2.0.291)

Download [joern-cli.zip](https://github.com/joernio/joern/releases/download/v2.0.291/joern-cli.zip) and extract it in /joern

#### Increse JVM heap size for joern 
Open the script of joern (joern/joern-cli/joern) and change last line to 
```
$SCRIPT -J-XX:+UseG1GC -J-XX:CompressedClassSpaceSize=128m -Dlog4j.configurationFile="$SCRIPT_ABS_DIR"/conf/log4j2.xml -J-XX:+UseStringDeduplication -J-Xmx12g "$@"
```
Speficly the -Xmx12g define the heap maximum size (2g,4g,8g,12g,16g...), even if you have only 8Gb of RAM you can use higher value (the system will use the swap area).

#### To see the heap usage and maximum capacity of a Java process
```
jstat -gccapacity <pid> 1000 10 
```
1000 10 represent the refresh rate and the number of total output to print

### Java JDK version 19
19 or previous version as well

Get fresh link from: https://jdk.java.net/19/

Download binary:

```
wget -P ./ https://download.java.net/java/GA/jdk19.0.2/fdb695a9d9064ad6b064dc6df578380c/7/GPL/openjdk-19.0.2_linux-x64_bin.tar.gz
```

Unpack it:
```
tar xvf openjdk-19.0.2_linux-x64_bin.tar.gz
```
Move to jvm folder:
```
mv jdk-19.0.2 /usr/lib/jvm/jdk-19.0.2
```
Update java and javac alternatives:
```
update-alternatives --install "/usr/bin/javac" "javac" "/usr/lib/jvm/jdk-19.0.2/bin/javac" 3
update-alternatives --install "/usr/bin/java" "java" "/usr/lib/jvm/jdk-19.0.2/bin/java" 3
update-alternatives --set "javac" "/usr/lib/jvm/jdk-19.0.2/bin/javac"
update-alternatives --set "java" "/usr/lib/jvm/jdk-19.0.2/bin/java"
```
Use to switch between versions:
```
update-alternatives --config java
```

### For IVDetect
Install dgl 2.3.0 (optional)
Pytorch 2.3.0
Torch-sparse 2.3.0
```
pip install torch-scatter torch-sparse -f https://data.pyg.org/whl/torch-2.3.0+${CUDA}.html
```
where ${CUDA} should be replaced by either `cpu`, `cu118`, or `cu121` depending on your PyTorch installation.

### Hyperparameters from 

configs.json

:

1. **Learning Rate**:
   - Path: `config['bertggnn']['learning_rate']`
   - Description: The learning rate controls how much to change the model parameters at each step of the optimization process.

2. **Weight Decay**:
   - Path: `config['bertggnn']['weight_decay']`
   - Description: Weight decay is a regularization technique that helps prevent overfitting by adding a penalty to the loss function based on the magnitude of the model parameters.

3. **Loss Lambda**:
   - Path: `config['bertggnn']['loss_lambda']`
   - Description: This parameter might be used to balance different components of the loss function.

4. **Gated Graph Convolution Arguments**:
   - Path: `config['bertggnn']['model']['gated_graph_conv_args']`
   - Description: Parameters for the Gated Graph Convolution layer, such as `out_channels`, `num_layers`, `aggr`, and `bias`.

5. **Convolutional Layer Arguments**:
   - Path: `config['bertggnn']['model']['conv_args']`
   - Description: Parameters for the convolutional layers, such as `in_channels`, `out_channels`, `kernel_size`, and `padding`.

6. **Embedding Size**:
   - Path: `config['bertggnn']['model']['emb_size']`
   - Description: The size of the embeddings generated by the model.

7. **Batch Size**:
   - Path: `config['process']['batch_size']`
   - Description: The number of samples processed before the model is updated.

8. **Number of Epochs**:
   - Path: `config['process']['epochs']`
   - Description: The number of complete passes through the training dataset.

9. **Patience**:
   - Path: `config['process']['patience']`
   - Description: The number of epochs to wait for an improvement in validation performance before stopping the training early.

10. **Dataset Ratio**:
    - Path: `config['process']['dataset_ratio']`
    - Description: The ratio of the dataset to be used for training, validation, and testing.

### Summary:

- **Learning Rate**: Controls the step size during optimization.
- **Weight Decay**: Regularization technique to prevent overfitting.
- **Loss Lambda**: Balances different components of the loss function.
- **Gated Graph Convolution Arguments**: Parameters for the Gated Graph Convolution layer.
- **Convolutional Layer Arguments**: Parameters for the convolutional layers.
- **Embedding Size**: Size of the embeddings generated by the model.
- **Batch Size**: Number of samples processed before updating the model.
- **Number of Epochs**: Number of complete passes through the training dataset.
- **Patience**: Number of epochs to wait for improvement before early stopping.
- **Dataset Ratio**: Ratio of the dataset used for training, validation, and testing.

By using command-line arguments to override specific hyperparameters, you can easily run experiments with different configurations without modifying the code or the base configuration file.