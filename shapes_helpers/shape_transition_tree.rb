# For this to work running make run, move these lines to test.rb
# and define the necessary object in obj_to_highlight
=begin
def obj_to_highlight
  B.new
end

require 'shape_transition_tree.rb'
=end

require "set"

def root_shape
  RubyVM.debug_shape(Object.new)
end

def iterate_over_shapes(root=root_shape)
  res = []
  root.edges.each do |_, child_shape|
    res << child_shape.id
    res += iterate_over_shapes(child_shape)
  end
  res
end

obj_to_highlight
@all_shape_ids = [0] + iterate_over_shapes

def create_transition_tree(shape, ids, edges)
  ids << id(shape)
  shape.edges.each do |edge_name, child_shape|
    if @all_shape_ids.include? child_shape.id
      ids, edges = create_transition_tree(child_shape, ids, edges)
      edges << edge(edge_name, shape.id, child_shape.id)
    end
  end
  [ids, edges]
end

def id(shape)
  color = ", color=red" if @id_set&.include? shape.id
  "    #{shape.id} [label=\"ID: #{shape.id}, Parent: #{shape.parent_id}\"#{color}]"
end

def edge(edge_name, parent_id, child_id)
  color = ", color=red" if @edge_set&.include? [parent_id, child_id]
  "    #{parent_id} -> #{child_id} [label=\"#{edge_name}\"#{color}]"
end

def crawl_coloring(obj)
  shape = RubyVM.debug_shape(obj)

  @edge_set = Set.new
  @id_set = [0].to_set

  while shape.id != root_shape.id
    @id_set << shape.id
    @edge_set << [shape.parent_id, shape.id]
    shape = shape.parent
  end
end

crawl_coloring(obj_to_highlight)
ids, edges = create_transition_tree(RubyVM.debug_shape(Object.new), [], [])

$stderr.puts "digraph {"
$stderr.puts ids.join("\n")
$stderr.puts ""
$stderr.puts edges.join("\n")
$stderr.puts "}"
