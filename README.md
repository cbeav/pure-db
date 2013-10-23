PureDB
======

PureDB seeks to be source-of-truth partitioned document storage accessible
via HTTP. This is a lofty goal, and it is currently just a thin HTTP wrapper
on top of MySQL. When completed, PureDB will have three tiers: a routing tier,
a storage tier, and an async tier. 

Routing tier:
-------------
Schemas will be housed in the routing tier, and shipped with requests through
the storage tier and onto the async tier. The routing tier will house
ownership information, and orchestrate two-phase commits over the storage
layer to persist data to all known replicas of a given partition.

Storage tier:
-------------
The goal of the storage tier is to deserialize as little as possible. Using
protobuf as the underlying storage format will support schema-unaware updates
with occasional compaction. As much as possible, the storage layer should be
bytes-in/bytes-out. To support read-after-write consistency, all storage
nodes are treated as source of truth for their partitions, and all commits
occur transactionally across all replicas (recommended 3). Accordingly,
GET requests can be routed to any replica, and deserialization will occur
in the client (unlimited free CPU).

Async tier:
-----------
The MySQL binary log will provide a change stream to support asynchronous
data slurp. This is for sure the least thought out tier, but the goal here
is to provide some mechanism for eventually consistent indexing or ETL to
Hadoop.


Author: Chris Beavers
