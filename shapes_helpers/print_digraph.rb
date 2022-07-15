tt = ObjectSpace.shape_transition_tree

def iterate_over_shape(shape_hash, ids, edges)
  ids << id(shape_hash)
  shape_hash[:edges].each do |edge|
    next_shape_hash = edge[1]
    edges << edge(shape_hash, next_shape_hash, edge[0])
    ids, edges = iterate_over_shape(next_shape_hash, ids, edges)
  end
  [ids, edges]
end

def id(shape_hash)
  "    #{shape_hash[:id]} [label=\"ID: #{shape_hash[:id]}, Parent: #{shape_hash[:parent_id]}\"]"
end

def edge(shape_hash, next_shape_hash, edge_var)
  "    #{shape_hash[:id]} -> #{next_shape_hash[:id]} [label=\"#{edge_var}\"]"
end

ids, edges = iterate_over_shape(tt, [], [])
$stderr.puts "digraph {"
$stderr.puts ids.join("\n")
$stderr.puts "\n"
$stderr.puts edges.join("\n")
$stderr.puts "}"
