// @(#)(>>>FILE<<<) -*- mode: graphviz-dot -*-
// Time-stamp: <Julian Qian 2011-04-21 11:36:05>
// Copyright (>>>YEAR<<<) (>>>USER_NAME<<<)
//
graph G {
    compound=true;
    // rankdir=LR;
	size="6,6";
    (>>>POINT<<<)
    // optional, node statement
    node [shape=box] box1 box2;
	node [shape=ellipse] {node [label="same"] same1 same2} ellipse;

    // usually, node define
    // default type is ellipse
    box1 box2 [style=filled, fillcolor=yellow];
    // define record type
    n1 [shape=record, label="{a|<f1>b|c}"];

    // subgraph statement
    subgraph cluster_1 {
        style=filled;
    	label="Sub Graph";
    	color=lightgrey;

        box1 -- same1 -- n1;
    }

    edge [color=red]
    ellipse -- same2 [lable="test line"];
    box2 -- same2 -- n1:f1 [style=dotted, color=blue];

    label = "\n\nSample Graph\ndrawn by (>>>USER_NAME<<<)";
    fontsize=20;
}
