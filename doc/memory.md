## Memory Layout in Ruby

### Overview
Ruby uses a garbage collector (GC) to manage memory. The GC automatically frees up memory that is no longer in use, preventing memory leaks and optimizing performance.
This page explains how Objects are stored in memory, and what optimizations are done by Ruby to improve efficiency.

### Object Header
The way Object are persisted in memory depends on the underlying processor architecture.
Here we are representing how memory is used for a x86_64 architecture.

Every Object has a 16-bytes header defined by the `RBasic` struct:

```
+-------------+-------------+
|    flags    |    klass    |
+-------------+-------------+
|<- 8 bytes ->|<- 8 bytes ->|
```

Where flags stores information about the Object, and klass is the reference to the Class object representing the class of the current object.

Flags are comprised of several distinct elements:
```
+----------------+------------------------------------+--------------------------+
|  Object type   |            ruby_fl_type            |         reserved         |
+----------------+------------------------------------+--------------------------+
|<- bits 0 to 5->|<---------- bits 5 to 30 ---------->|<---- bits 31 to 63 ----->|
```

Object type indicates the type of the Object (Class, Array, Hash, Float...).
`ruby_fl_type` are flags used by the Ruby VM and the Garbage collector. For example, when an Object is frozen, the `RUBY_FL_FREEZE` flag is set.

### Heaps

To keep Objects in memory, Ruby uses 5 fixed-size slot heaps, and one general heap.

If the Object is less than 640 bytes, then the object can be stored in one of the 5 fixed-size slot heaps.
The heap selected is the smallest one that can contain the object:

| Slot ID |  Slot size |
| -- | -- |
| 0 | 40 bytes |
| 1 | 80 bytes |
| 2 | 160 bytes |
| 3 | 320 bytes |
| 4 | 640 bytes |

If the object is larger than 640 bytes, then the object is kept in the general heap.

The benefit of using fixed-size slot heaps is to avoid memory fragmentation. When an object is freed, the slot can be reused by another object without fearing memory overlap.
The general heap on the other hand will become fragmented over time and need to be compacted at some point to ensure further objects can still be kept in memory.

### Optimizations
Since the smallest slot size is 40 bytes, and the object header is only 16 bytes. It means there's an additional 24 bytes free to be used by the object.

Each object type try to use these 24 bytes to persist their data when possible.
For example, the Array type will allow keeping 3 values in there:
```
+-------------+-------------+-------------+-------------+-------------+
|    flags    |    klass    | idx 0 value | idx 1 value | idx 2 value |
+-------------+-------------+-------------+-------------+-------------+
|<- 8 bytes ->|<- 8 bytes ->|<- 8 bytes ->|<- 8 bytes ->|<- 8 bytes ->|
|<----------------------------- 40 bytes ---------------------------->|
```

