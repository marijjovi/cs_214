#lang dssl2

# HW4: Graph
let eight_principles = ["Know your rights.",
    "Acknowledge your sources.",
    "Protect your work.",
    "Avoid suspicion.",
    "Do your own work.",
    "Never falsify a record or permit another person to do so.",
    "Never fabricate data, citations, or experimental results.",
    "Always tell the truth when discussing your work with your instructor."]
import cons
import 'hw4-lib/dictionaries.rkt'


###
### REPRESENTATION
###

# A Vertex is a natural number.
let Vertex? = nat?

# A VertexList is either
#  - None, or
#  - cons(v, vs), where v is a Vertex and vs is a VertexList
let VertexList? = Cons.ListC[Vertex?]

# A Weight is a real number. (It’s a number, but it’s neither infinite
# nor not-a-number.)
let Weight? = AndC(num?, NotC(OrC(inf, -inf, nan)))

# An OptWeight is either
# - a Weight, or
# - None
let OptWeight? = OrC(Weight?, NoneC)

# A WEdge is WEdge(Vertex, Vertex, Weight)
struct WEdge:
    let u: Vertex?
    let v: Vertex?
    let w: Weight?

# A WEdgeList is either
#  - None, or
#  - cons(w, ws), where w is a WEdge and ws is a WEdgeList
let WEdgeList? = Cons.ListC[WEdge?]

# A weighted, undirected graph ADT.
interface WUGRAPH:

    # Returns the number of vertices in the graph. (The vertices
    # are numbered 0, 1, ..., k - 1.)
    def len(self) -> nat?

    # Sets the weight of the edge between u and v to be w. Passing a
    # real number for w updates or adds the edge to have that weight,
    # whereas providing providing None for w removes the edge if
    # present. (In other words, this operation is idempotent.)
    def set_edge(self, u: Vertex?, v: Vertex?, w: OptWeight?) -> NoneC

    # Gets the weight of the edge between u and v, or None if there
    # is no such edge.
    def get_edge(self, u: Vertex?, v: Vertex?) -> OptWeight?

    # Gets a list of all vertices adjacent to v. (The order of the
    # list is unspecified.)
    def get_adjacent(self, v: Vertex?) -> VertexList?

    # Gets a list of all edges in the graph, in an unspecified order.
    # This list only includes one direction for each edge. For
    # example, if there is an edge of weight 10 between vertices
    # 1 and 3, then exactly one of WEdge(1, 3, 10) or WEdge(3, 1, 10)
    # will be in the result list, but not both.
    def get_all_edges(self) -> WEdgeList?

class WUGraph (WUGRAPH):
    let size
    let data
    
    

    def __init__(self, size: nat?):
        self.size = size
        self.data = [None; max(0, self.size)]
        for i in range(self.size):
            self.data[i] = [None; max(0, self.size)]
        
        
        
        

# Other methods you may need can go here.
    def len(self):
        return self.size
    
    def set_edge(self, u, v, w):
       let one = self.data[u]
       one[v] = w 
       let two = self.data[v]
       two[u] = w
        
        
    def get_edge(self, u, v):
       let one = self.data[u]
       return one[v]
    
    def get_adjacent(self, v):
        let box = self.data[v]
        let Listy = None
        for i in range(len(self.data)):
            let val = box[i]
            if val != None:
                Listy = cons(i, Listy)
        return Listy
                
    def get_all_edges(self):
        let Wedges = None
        for i in range(len(self.data)):
            let box = self.data[i] 
            for l in range(i, len(box)):
                let val = box[l]
                if val != None: 
                      let new_wed = WEdge(i, l, val)
                      if Wedges == None:
                          Wedges = cons(new_wed, None)
                      elif Wedges != None:    
                          Wedges = cons(new_wed, Wedges)
        return Wedges
                          
                               
                  
               

###
### List helpers
###

# To test methods that return lists with elements in an unspecified
# order, you can use these functions for sorting. Sorting these lists
# will put their elements in a predictable order, order which you can
# use when writing down the expected result part of your tests.

# sort_vertices : ListOf[Vertex] -> ListOf[Vertex]
# Sorts a list of numbers.
def sort_vertices(lst: Cons.list?) -> Cons.list?:
    def vertex_lt?(u, v): return u < v
    return Cons.sort[Vertex?](vertex_lt?, lst)

# sort_edges : ListOf[WEdge] -> ListOf[WEdge]
# Sorts a list of weighted edges, lexicographically
# ASSUMPTION: There's no need to compare weights because
# the same edge can’t appear with different weights.
def sort_edges(lst: Cons.list?) -> Cons.list?:
    def edge_lt?(e1, e2):
        return e1.u < e2.u or (e1.u == e2.u and e1.v < e2.v)
    return Cons.sort[WEdge?](edge_lt?, lst)

###
### BUILDING GRAPHS
###

def example_graph() -> WUGraph?:
    let result = WUGraph(6) # 6-vertex graph from the assignment
    result.set_edge(0, 1, 12)
    result.set_edge(1, 2, 31)
    result.set_edge(1, 3, 56)
    result.set_edge(2, 5, 7)
    result.set_edge(2, 4, -2)
    result.set_edge(3, 5, 1)
    result.set_edge(3, 4, 9)
    return result
    

struct CityMap:
    let graph
    let city_name_to_node_id
    let node_id_to_city_name

def my_neck_of_the_woods():
    
    
    let A = AssociationList()
    A.put("Sarajevo", 0)
    A.put("Zenica", 1)
    A.put("Zavidovici", 2)
    A.put("Zepce", 3)
    A.put("Siroki Brijeg", 4)
    
    let B = AssociationList()
    B.put(0, "Sarajevo")
    B.put(1, "Zenica")
    B.put(2, "Zavidovici")
    B.put(3, "Zepce")
    B.put(4, "Siroki Brijeg")
    
    let g = WUGraph(5)
    g.set_edge(0, 1, 69)
    g.set_edge(0, 2, 121)
    g.set_edge(0, 3, 109)
    g.set_edge(0, 4, 146)
    g.set_edge(1,2, 51)
    g.set_edge(1,3, 39)
    g.set_edge(1, 4, 197)
    g.set_edge(2,3, 12)
    g.set_edge(2,4, 249)
    g.set_edge(3,4, 236)
    
    let map = CityMap(g, A, B)
    return map 
    
#   ^ YOUR WORK GOES HERE

###
### DFS
###

# dfs : WUGRAPH Vertex [Vertex -> any] -> None
# Performs a depth-first search starting at `start`, applying `f`
# to each vertex once as it is discovered by the search.
def dfs(graph: WUGRAPH!, start: Vertex?, f: FunC[Vertex?, AnyC]) -> NoneC:
    let tof = [False; max(0, len(graph))]
    def application(start, f):
        if tof[start] == False: 
            f(start)
            tof[start] = True 
            let funct = graph.get_adjacent(start)
            while funct != None:
                application(funct.data, f)
                funct = funct.next
    application(start, f)
            
    
    
#   ^ YOUR WORK GOES HERE
    ## create a vector holding false vals 
    ## if visited turn true 
    ## add to list 
    ## go the next thing in that one's vertexes
    ## once all are visited, return back up 
    
    

# dfs_to_list : WUGRAPH Vertex -> ListOf[Vertex]
# Performs a depth-first search starting at `start` and returns a
# list of all reachable vertices.
#
# This function uses your `dfs` function to build a list in the
# order of the search. It will pass the test below if your dfs visits
# each reachable vertex once, regardless of the order in which it calls
# `f` on them. However, you should test it more thoroughly than that
# to make sure it is calling `f` (and thus exploring the graph) in
# a correct order.
def dfs_to_list(graph: WUGRAPH!, start: Vertex?) -> VertexList?:
    let list = None
    # Add to the front when we visit a node
    dfs(graph, start, lambda new: list = cons(new, list))
    # Reverse to the get elements in visiting order.
    return Cons.rev(list)

###
### TESTING
###

## You should test your code thoroughly. Here is one test to get you started:

test 'dfs_to_list(example_graph())':
    # Cons.from_vec is a convenience function from the `cons` library that
    # allows you to write a vector (using the nice vector syntax), and get
    # a linked list with the same elements.
    assert sort_vertices(dfs_to_list(example_graph(), 0)) == Cons.from_vec([0, 1, 2, 3, 4, 5])

test 'graph set up':
    let o = WUGraph(4)
    o.set_edge(1, 2, 45)
    o.set_edge(2, 3, 12)
    assert o.get_edge(3, 2) == 12
    assert o.get_edge(2,0) == None
    o.set_edge(2,1, None)
    assert o.get_edge(1,2) == None
    o.set_edge(0,0, 14)
    o.set_edge(1, 0 , 34)
    assert  sort_vertices(o.get_adjacent(0)) == cons{data: 0, next: cons {data: 1, next: None}}
    o.set_edge( 2, 0 , 15)
    assert  sort_vertices(o.get_adjacent(0)) ==  cons{data: 0, next: cons {data: 1, next: cons {data: 2, next: None}}}
    o.set_edge(1,0, None)
    assert  sort_vertices(o.get_adjacent(0)) == cons{data: 0, next: cons {data: 2, next: None}}
    assert sort_edges(o.get_all_edges()) == cons{data: WEdge {u: 0, v: 0, w: 14}, next: cons {data: WEdge {u: 0, v: 2, w: 15}, next: cons {data: WEdge {u: 2, v: 3, w: 12}, next: None}}}
    
    
test 'test type 2':
    let q = WUGraph(6)
    assert q.get_edge(4,1) == None 
    q.set_edge(2,1, 26)
    q.set_edge(3, 0, 43) 
    q.set_edge(5, 5, 11)
    assert q.get_edge(5,5) == 11
    q.set_edge(2,1, None)
    q.set_edge(3, 0, None)
    q.set_edge(5, 5, None)
    assert q.get_adjacent(3) == None 
    assert q.get_all_edges() == None
    
    
test "woods":
    let M = my_neck_of_the_woods()
    assert M.graph.get_edge(2,1) == 51
    assert M.city_name_to_node_id.get("Zepce") == 3
    assert M.node_id_to_city_name.get(2) == "Zavidovici"
    assert M.graph.get_edge(M.city_name_to_node_id.get("Zepce"), M.city_name_to_node_id.get("Siroki Brijeg")) == 236
    
    
    
    
    