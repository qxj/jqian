/* @(#)(>>>FILE<<<)
 * Time-stamp: < 2011-10-24 09:57:43>
 * Copyright (>>>YEAR<<<) (>>>USER_NAME<<<)
 * Author: (>>>AUTHOR<<<)
 * Version: $Id: (>>>FILE<<<),v 0.0 (>>>VC_DATE<<<) (>>>LOGIN_NAME<<<) Exp $
 */

package net.jqian;

import java.io.IOException;
import java.util.*;
import org.apache.hadoop.fs.*;
import org.apache.hadoop.conf.*;
import org.apache.hadoop.io.*;
import org.apache.hadoop.mapreduce.*;
import org.apache.hadoop.mapreduce.lib.input.*;
import org.apache.hadoop.mapreduce.lib.output.*;
import org.apache.hadoop.util.*;


public class (>>>FILE_SANS<<<)  extends Configured implements Tool {

    public static class (>>>FILE_SANS<<<)Mapper extends Mapper <LongWritable, Text, ${MapOutKey}, ${MapOutValue}> {

        public void map(LongWritable key, Text value, Context context) throws IOException, InterruptedException {
            // ...

        }
    }

    public static class (>>>FILE_SANS<<<)Reducer extends Reducer<${MapOutKey}, ${MapOutValue}, ${OutKey}, ${OutValue}> {

        public void reduce(${MapOutKey} key, Iterable <${MapOutValue}> values,
                Context context) throws IOException, InterruptedException {

        }
    }

    @Override
    public int run(String[] args) throws Exception {
        Configuration conf = getConf();
        conf.set("mapred.job.name", " ");
        conf.set("mapred.job.queue.name", "gdm");
        conf.set("mapred.map.tasks", "5");
        // conf.set("mapred.reduce.tasks", "200");

        Job job = new Job(conf);
        job.setJarByClass((>>>FILE_SANS<<<).class);

        Path inputPath = new Path(args[0]);
        Path outputPath = new Path(args[1]);

        FileInputFormat.setInputPaths(job, inputPath);
        FileOutputFormat.setOutputPath(job, outputPath);

        job.setMapOutputKeyClass(${MapOutKey}.class);
        job.setMapOutputValueClass(${MapOutValue}.class);
        job.setOutputKeyClass(${OutKey}.class);
        job.setOutputValueClass(${OutValue}.class);
        job.setInputFormatClass(TextInputFormat.class);
        job.setOutputFormatClass(TextOutputFormat.class);
        job.setMapperClass((>>>FILE_SANS<<<)Mapper.class);
        job.setReducerClass((>>>FILE_SANS<<<)Reducer.class);

        return job.waitForCompletion(true) ? 0 : 1;
    }

    public static void main(String[] args) throws Exception {
        int exitCode = ToolRunner.run(new (>>>FILE_SANS<<<)(), args);
        System.exit(exitCode);
    }
}
