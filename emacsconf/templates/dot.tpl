graph G {                       // alternative: digraph
	size="6,6";
    // node statement
    node [style=rounded];
    node [shape=box] box1 box2;
	node [shape=ellipse] {node [label="same"] same1 same2} ellipse;
    // node define
    box1 box2 [style=filled, fillcolor=yellow];
    n1 [shape=record, label="{a|<f1>b|c}"];

    subgraph cluster_1 {
        style=filled;
        box1 -- same1 -- n1;
    	label="Sub Graph";
    	color=lightgrey;
    }

    ellipse -- same2 [lable="test line"];
    box2 -- same2 -- n1:f1;

    label = "\n\nEntity Relation Diagram\ndrawn by NEATO";
    fontsize=20;
}