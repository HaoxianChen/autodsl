# Problem specification

[Full example directory](reachable).

## 1. Declare types (optional)
This is an optional step.
[Souffle](https://souffle-lang.github.io/types) 
naturally supports ``number`` and ``symbol`` types.

Custom types can be specified by adding following lines: 

```.type Node <: symbol ```

Each type specifies the typename, 
and it should be a subtype of either ``symbol`` or ``number``.

## 2. Declare relations
Each relation is declared in a line:

```
.decl *link(x: Node,y:Node) 
.decl reachable(x: Node, y: Node)
```
where input relations are annotated with a ``*`` 
before the relation name.

## 3. Specify input output examples
For every input relation ``rel_in``, 
prepare a file named ``rel_in.facts`` to store
input tuples.
For example, ``link.facts``:

```
a b
b c
a c
```
specifies three tuples for the ``link`` relation: 
``link(a,b), link(b,c), link(a,c)``.

Similarly, for every output relation ``rel_out``,
specify tuples in a file named ``rel_out.csv``. 

## 4. Multiple (EDB, IDB) pairs

To specify multiple (EDB, IDB) pairs, 
add a field named ``InstanceId`` in every relation.
For every tuple, specify the ID of the (EDB, IDB) 
pair it belongs to.

For example, the same ``link`` and ``reachable`` relation
specification now becomes:
``` 
.decl *link(x: Node,y:Node, i:InstanceId) 
.decl reachable(x: Node, y: Node, i:InstanceId)
```

And to support two pairs of (EDB, IDB) 
the content of ``link.facts`` becomes:
```
# First pair
a b 1
b c 1
a c 1

# Second pair
a b 2
c d 2
```

See [benchmarks/locality](locality) for a complete example.