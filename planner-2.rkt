#lang dssl2

# Final project: Trip Planner
let eight_principles = ["Know your rights.",
    "Acknowledge your sources.",
    "Protect your work.",
    "Avoid suspicion.",
    "Do your own work.",
    "Never falsify a record or permit another person to do so.",
    "Never fabricate data, citations, or experimental results.",
    "Always tell the truth when discussing your work with your instructor."]
    
import cons
import sbox_hash 
import  'project-lib/dictionaries.rkt'
import 'project-lib/binheap.rkt'
import 'project-lib/graph.rkt'
import 'project-lib/stack-queue.rkt'

### Basic Types ###

#  - Latitudes and longitudes are numbers:
let Lat?  = num?
let Lon?  = num?

#  - Point-of-interest categories and names are strings:
let Cat?  = str?
let Name? = str?

### Raw Item Types ###

#  - Raw positions are 2-element vectors with a latitude and a longitude
let RawPos? = TupC[Lat?, Lon?]

#  - Raw road segments are 4-element vectors with the latitude and
#    longitude of their first endpoint, then the latitude and longitude
#    of their second endpoint
let RawSeg? = TupC[Lat?, Lon?, Lat?, Lon?]

#  - Raw points-of-interest are 4-element vectors with a latitude, a
#    longitude, a point-of-interest category, and a name
let RawPOI? = TupC[Lat?, Lon?, Cat?, Name?]

### Contract Helpers ###

# ListC[T] is a list of `T`s (linear time):
let ListC = Cons.ListC
# List of unspecified element type (constant time):
let List? = Cons.list?

struct coordinate:
    let lat: num?
    let lon: num?
    

##struct road:
##    let p1 : coordinate ## maybe edit this 
##    let p2 : coordinate 
    
struct location: 
    let name: str?
    let category: str?
    let position: nat? ## node value in tree 
        

struct dijkstruct:
    let v: num?
    let w: num?
    
## Copy of the mem? function from the dictionary ADT.     
def fake_mem(list, key):
        let starter = list
        let i = 0 
        while i < Cons.len(list):
            if starter.data == key: return True
            if starter.next == None: return False
            else:
                starter = starter.next
        return False  

interface TRIP_PLANNER:

    # Returns the positions of all the points-of-interest that belong to
    # the given category.
    def locate_all(
            self,
            dst_cat:  Cat?           # point-of-interest category
        )   ->        ListC[RawPos?] # positions of the POIs

    # Returns the shortest route, if any, from the given source position
    # to the point-of-interest with the given name.
    def plan_route(
            self,
            src_lat:  Lat?,          # starting latitude
            src_lon:  Lon?,          # starting longitude
            dst_name: Name?          # name of goal
        )   ->        ListC[RawPos?] # path to goal

    # Finds no more than `n` points-of-interest of the given category
    # nearest to the source position.
    def find_nearby(
            self,
            src_lat:  Lat?,          # starting latitude
            src_lon:  Lon?,          # starting longitude
            dst_cat:  Cat?,          # point-of-interest category
            n:        nat?           # maximum number of results
        )   ->        ListC[RawPOI?] # list of nearby POIs

def calculator(cord1, cord2):
     let one = (cord1.lat - cord2.lat) 
     let two = (cord1.lon - cord2.lon) 
     let squares = (one *one) + (two *two)
     return squares.sqrt()               
    
        
class TripPlanner (TRIP_PLANNER):
    let roads
    let cords_to_nodes 
    let nodes_to_cords 
    let locations 
    let categories
#   ^ YOUR WORK GOES HERE
    def __init__(self, road, loc):   
        ## set up two dictionaries for bi directional mapping 
        # one is a hash table, another is an array 
        let n_coordinates = road.len() * 2
        let coords_to_node_ids = HashTable(n_coordinates, make_sbox_hash())
        let node_ids_to_coords = [None; n_coordinates]
        
        ## each road segment is two positions, so they have to 
        ##divided up properly to be placed as individual coordinates 
        ## within the future road graphs as well as locations in the 
        ## poi struct 
        for i, r in road:
            let f = i * 2
            
            let this_coord = coordinate(r[0], r[1])
            coords_to_node_ids.put(this_coord, f)
            let that_coord = coordinate(r[2], r[3])
            coords_to_node_ids.put(that_coord, f + 1)
                 
        let road_graph = WUGraph(n_coordinates)
        ## mapping from cord to node and node to cord must be seperate bc cords may have 
        ## been repeated in the first graphing. this is fine, as the values are overwritten 
        ## and their final node value is accessed here, and then the coord value is stored at its
        ## final node
        for r in road:
            let node_1 = coords_to_node_ids.get(coordinate(r[0], r[1]))
            node_ids_to_coords[node_1] = coordinate(r[0], r[1])
            let node_2 = coords_to_node_ids.get(coordinate(r[2], r[3]))
            node_ids_to_coords[node_2] = coordinate(r[2], r[3])
            road_graph.set_edge(node_1, node_2, calculator(coordinate(r[0], r[1]),coordinate(r[2], r[3])))
              
        let n_loc = loc.len()
        let list_of_cats = None
        let locations = HashTable(n_loc, make_sbox_hash())
        let categories = HashTable(n_loc, make_sbox_hash())
        ## making location structes and placing them in the location hash table 
        ## choice to use node as position was to make the connection to graphing easier 
        for l in loc:
            let node_val = coords_to_node_ids.get(coordinate(l[0], l[1]))
            let this_loc = location(l[3], l[2], node_val)
            locations.put(this_loc.name, this_loc)
            let cat_check = this_loc.category 
            let val = categories.mem?(cat_check)
            ## making a dictionary of categories, where the key is the category name 
            ## and the value is a cons list of all the names of pois in that cat
            let listy = None
            if val:
                listy = categories.get(cat_check)
                listy = cons(this_loc.name, listy)   
            if not val:
                listy = cons(this_loc.name, None)
            categories.put(cat_check, listy)
        ## assigning everything that i'll be using     
        self.roads = road_graph
        self.locations = locations
        self.cords_to_nodes = coords_to_node_ids
        self.nodes_to_cords = node_ids_to_coords
        self.categories = categories 
   
        
    
           
    def locate_all(self, cat: str?):
        ## check if there exist pois of the category 
        let here? = self.categories.mem?(cat)
        if not here?:
            return None
        let names = self.categories.get(cat)
        
        
        
        let returners = None
        let i = 0
        let name = names.data
        let copy = names
        while i < Cons.len(names): 
            ## get the necessary info from the locations dict 
            let pos = self.locations.get(name).position
            
            let cords = self.nodes_to_cords.get(pos)
            
            let latty = cords.lat
            let longy = cords.lon
            let val = [latty,longy]
            
            ## after formatting properly, add to list and return 
            if returners == None:
                returners = cons(val, None)
            elif returners != None:
                if not fake_mem(returners,val):
                    returners = cons(val, returners)
            if copy.next == None: return returners
            copy = copy.next
            name = copy.data
            
            i = i + 1
        return returners 
            
    def deeks_help(self, start, end):
       
        let dist = [inf; self.roads.len()]
        let pred = [None; self.roads.len()] ## dijkstras in the flesh !  
        dist[start] = 0 
        pred[start] = start
        let todo = BinHeap(self.roads.len(), lambda x, y: x.w<y.w)
        let done = [False; self.roads.len()]
        todo.insert(dijkstruct(start, 0)) 
        # note : i switched up the order of insert values, so its node and then weight 
        while todo.len() != 0:
            let v = todo.find_min()
            todo.remove_min()
            
            if done[v.v] == False:
                done[v.v] = True
                let outgoers = self.roads.get_adjacent(v.v)
                
                if outgoers != None:
                    let copy = outgoers
                    let edge = outgoers.data
                    let i = 0 
                
                    while i < Cons.len(outgoers):
                        
                        let weight = self.roads.get_edge(v.v, edge)
                        if dist[v.v] + weight < dist[edge]: ## replacing v.w with weight in roads
                            dist[edge] = dist[v.v] + weight ## replaces v.w with road weight 
                            
                            pred[edge] = v.v
                        
                            todo.insert(dijkstruct( edge , dist[edge]))
                            
                        if copy.next != None:
                            copy = copy.next
                            edge = copy.data
                        i = i + 1 
                        
        ## return the preds vector, useful for both functions i'm using this for              
        return pred
                        
        
        
    def plan_route(self, lat_val, lon_val, loc_name):
        
        let start = self.cords_to_nodes.get(coordinate(lat_val, lon_val))
        
        if not self.locations.mem?(loc_name): return None
        let end = self.locations.get(loc_name).position 
        ## get start and end nodes ! 
        if (start == end):
            return cons([lat_val,lon_val], None)
        ## if the start and end are the same, we don't have to 
        ## look for anything! already there 
        let follow_through = self.deeks_help(start, end)
        if follow_through[end] == None: return None
        ## if not found, then there is no path 
        ## return immediately 
        let list_nodes = cons(end, None)
        let looper = end 
        while looper != start:
            let pred = follow_through[looper]
            list_nodes = cons(pred, list_nodes)
            looper = pred
        # list_nodes is the list of nodes in the shortest path 
        let final_list = None
        
        let copy = list_nodes
        ## turn nodes to coord vals, add to list 
        while copy != None:
            let node = copy.data
            let cord = self.nodes_to_cords.get(node)
            
            let return_val = [cord.lat, cord.lon]
            final_list = cons( return_val ,final_list)
            copy = copy.next 
        ## return reversed to get accurate order     
        return Cons.rev(final_list)
            
            
        
            
        
        
    def find_nearby(self, lon, lat, cat, numback):
        let all_poi = self.locate_all(cat)
        # if no locations in the cat, return nun 
        if all_poi == []: return None
        let copy = all_poi 
        let dict = AssociationList()
        
        # loop through all positions under that category
        while copy != None:
            let node = copy.data
            let new_key = self.cords_to_nodes.get(coordinate(node[0], node[1]))
            #make a dict w the node value as a key and 0 as the value,
            #  we'll add to it 
            dict.put(new_key,0)
            copy = copy.next 
        # loop through all positions again 
        
        let copy_2 = all_poi
        
        let start = self.cords_to_nodes.get(coordinate(lon,lat))
        let actually_reached = None
        ## actually reached is to ensure our key values are actually 
        ## reached, so we can add them later 
        while copy_2 != None:
           let measure = copy_2.data
           # get shortest path using deeks 
           let end = self.cords_to_nodes.get(coordinate(measure[0], measure[1]))
           let listicle =  self.deeks_help(start, end)
           
           let looper = end
           #loop through predcessors list 
           
           
           if listicle[end] == None:
               copy_2 = copy_2.next
           else:
               while looper != start:
                   let pred = listicle[looper]
                   #loop up weight from node to preds 
                   
                   
                   let weight = self.roads.get_edge(looper, pred)
                   let old_val = dict.get(end)
                   #add weight to dict key 
                   dict.put(end, old_val + weight)
                   looper = pred
               actually_reached = cons(measure, actually_reached)    
               copy_2 = copy_2.next
        #now we make a BinHeap and sort by weight 
        let binny = BinHeap(Cons.len(all_poi), lambda x, y: x.w<y.w)
        ## using binheap, using deekstructs, to return smallest weights 
        ## so we get the closest values 
        let copy_3 = actually_reached
        # looping through the values that actually can be reached from 
        # our start 
        while copy_3 != None:
            let key_cord = copy_3.data
            
            let key = self.cords_to_nodes.get(coordinate(key_cord[0],key_cord[1]))
            let weighty = dict.get(key)
            binny.insert(dijkstruct(key,weighty))
            copy_3 = copy_3.next
            
       #for the n min, return node, look up val, and add to list 
        let return_list = None 
        let i = 0 
        let moo = 0 
        if Cons.len(actually_reached) >= numback:
            moo = numback
        else:
            moo = Cons.len(actually_reached)
        # checking if we actually have that many positions in the 
        # category, if not, return how many we actually have 
        while i < moo:
            
            let node = binny.find_min().v
            binny.remove_min()
            let locations = self.categories.get(cat)
            let copy_4 = locations
            ## get min, remove min and look up its poi 
            ## add to list for return 
            while copy_4 != None:
                let name = copy_4.data
                
                let comp_val = self.locations.get(name)
                if node == comp_val.position:
                    let cord = self.nodes_to_cords.get(node)
                    
                    return_list = cons([cord.lat, cord.lon, comp_val.category, comp_val.name], return_list)
                    i = i +1
                    ## breaking just in case we have two in that same spot 
                    break
                copy_4 = copy_4.next
            
        return return_list
       
def my_first_example():
    return TripPlanner([[0,0, 0,1], [0,0, 1,0]],
                       [[0,0, "bar", "The Empty Bottle"],
                        [0,1, "food", "Pierogi"]])

test 'My first locate_all test':
    assert my_first_example().locate_all("food") == \
        cons([0,1], None)

test 'My first plan_route test':
   assert my_first_example().plan_route(0, 0, "Pierogi") == \
       cons([0,0], cons([0,1], None))

test 'My first find_nearby test':
   assert my_first_example().find_nearby(0, 0, "food", 1) == \
        cons([0,1, "food", "Pierogi"], None)
        
test "no POI":
    let tp = TripPlanner(
    [[0,0,1,0]], 
    [])
    let result = tp.locate_all('bank')
    assert result == None ## return empty cons list, not empty vector 
    
test "single POI, wrong category":
    let tp = TripPlanner(
      [[0, 0, 1, 0]],
      [[1, 0, 'bank', 'Union']])
    let result = tp.locate_all('food')
    
    assert result == None

test "2 POIs, 1 in relevant category":
    let tp = TripPlanner(
      [[0, 0, 1.5, 0],
       [1.5, 0, 2.5, 0],
       [2.5, 0, 3, 0]],
      [[1.5, 0, 'bank', 'Union'],
       [2.5, 0, 'barber', 'Tony']])
    let result = tp.locate_all('barber')
    
    assert result == cons {data: [2.5, 0], next: None}
    
test "4 POIs, 2 in relevant category":
      let tp = TripPlanner(
      [[0, 0, 1.5, 0],
       [1.5, 0, 2.5, 0],
       [2.5, 0, 3, 0],
       [4, 0, 5, 0]],
      [[1.5, 0, 'bank', 'Union'],
       [3, 0, 'barber', 'Tony'],
       [4, 0, 'food', 'Jollibee'],
       [5, 0, 'barber', 'Judy']])
      let result = tp.locate_all('barber')
      ##println(tp.categories)
      assert result == cons {data: [3, 0], next: cons {data: [5, 0], next: None}}
      
      
test "multiple POI in same location":
    let tp = TripPlanner(
      [[0, 0, 1.5, 0],
       [1.5, 0, 2.5, 0],
       [2.5, 0, 3, 0],
       [4, 0, 5, 0],
       [3, 0, 4, 0]],
      [[1.5, 0, 'bank', 'Union'],
       [3, 0, 'barber', 'Tony'],
       [5, 0, 'bar', 'Pasta'],
       [5, 0, 'barber', 'Judy'],
       [5, 0, 'food', 'Jollibee']])
    let result = tp.locate_all('bar')
    
    assert result == cons {data: [5, 0], next: None}
    
test "multiple POI in same loc":
    let tp = TripPlanner(
      [[0, 0, 1.5, 0],
       [1.5, 0, 2.5, 0],
       [2.5, 0, 3, 0],
       [4, 0, 5, 0],
       [3, 0, 4, 0]],
      [[1.5, 0, 'bank', 'Union'],
       [3, 0, 'barber', 'Tony'],
       [5, 0, 'barber', 'Judy'],
       [5, 0, 'bar', 'Pasta'],
       [5, 0, 'food', 'Jollibee']])
    let result = tp.locate_all('bar')
    
    assert result == cons {data: [5, 0], next: None}
    
test "multpile POI, 2 rel":
     let tp = TripPlanner(
      [[0, 0, 1.5, 0],
       [1.5, 0, 2.5, 0],
       [2.5, 0, 3, 0],
       [4, 0, 5, 0],
       [3, 0, 4, 0]],
      [[1.5, 0, 'bank', 'Union'],
       [3, 0, 'barber', 'Tony'],
       [5, 0, 'bar', 'Pasta'],
       [5, 0, 'barber', 'Judy'],
       [5, 0, 'food', 'Jollibee']])
     let result = tp.locate_all('barber')
   
     assert result == cons {data: [3, 0], next: cons {data: [5, 0], next: None}}

test "same locations":
    let tp = TripPlanner(
      [[0, 0, 1.5, 0],
       [1.5, 0, 2.5, 0],
       [2.5, 0, 3, 0],
       [4, 0, 5, 0],
       [3, 0, 4, 0]],
      [[1.5, 0, 'bank', 'Union'],
       [3, 0, 'barber', 'Tony'],
       [5, 0, 'barber', 'Judy'],
       [5, 0, 'barber', 'Lily']])
    let result = tp.locate_all('barber')
    
    assert result == cons {data: [3, 0], next: cons {data: [5, 0], next: None}}
    
    
test "2-step route":
    let tp = TripPlanner(
      [[0, 0, 1.5, 0],
       [1.5, 0, 2.5, 0],
       [2.5, 0, 3, 0]],
      [[1.5, 0, 'bank', 'Union'],
       [2.5, 0, 'barber', 'Tony']])
    let result = tp.plan_route(0, 0, 'Tony')
    assert result == cons{data:[0,0], next: cons{data:[1.5,0], next: cons{data:[2.5,0], next: None}}}
    
    
test "3-step route":
    let tp = TripPlanner(
      [[0, 0, 1.5, 0],
       [1.5, 0, 2.5, 0],
       [2.5, 0, 3, 0]],
      [[1.5, 0, 'bank', 'Union'],
       [3, 0, 'barber', 'Tony']])
    let result = tp.plan_route(0, 0, 'Tony')
    assert result == cons {data: [0, 0], next: cons {data: [1.5, 0], next: cons {data: [2.5, 0], next: cons {data: [3, 0], next: None}}}}
    
    
test "from barber to bank":
    let tp = TripPlanner(
      [[0, 0, 1.5, 0],
       [1.5, 0, 2.5, 0],
       [2.5, 0, 3, 0]],
      [[1.5, 0, 'bank', 'Union'],
       [3, 0, 'barber', 'Tony']])
    let result = tp.plan_route(3, 0, 'Union')
    assert result == cons {data: [3, 0], next: cons {data: [2.5, 0], next: cons {data: [1.5, 0], next: None}}}
    
test "0-step route":
    let tp = TripPlanner(
      [[0, 0, 1, 0]],
      [[0, 0, 'bank', 'Union']])
    let result = tp.plan_route(0, 0, 'Union')
    assert result == cons {data : [0,0], next: None}
    
test "Destination doesn't exist":
    let tp = TripPlanner(
      [[0, 0, 1.5, 0],
       [1.5, 0, 2.5, 0],
       [2.5, 0, 3, 0]],
      [[1.5, 0, 'bank', 'Union'],
       [3, 0, 'barber', 'Tony']])
    let result = tp.plan_route(0, 0, 'Judy')
    assert result == None
    
test "Destination isn't reachable":
    let tp = TripPlanner(
      [[0, 0, 1.5, 0],
       [1.5, 0, 2.5, 0],
       [2.5, 0, 3, 0],
       [4, 0, 5, 0]],
      [[1.5, 0, 'bank', 'Union'],
       [3, 0, 'barber', 'Tony'],
       [5, 0, 'barber', 'Judy']])
    let result = tp.plan_route(0, 0, 'Judy')
    assert result == None
    
test "BFS is not SSSP (route)":
     let tp = TripPlanner(
      [[0, 0, 0, 9],
       [0, 9, 9, 9],
       [0, 0, 1, 1],
       [1, 1, 2, 2],
       [2, 2, 3, 3],
       [3, 3, 4, 4],
       [4, 4, 5, 5],
       [5, 5, 6, 6],
       [6, 6, 7, 7],
       [7, 7, 8, 8],
       [8, 8, 9, 9]],
      [[7, 7, 'haberdasher', 'Archit'],
       [8, 8, 'haberdasher', 'Braden'],
       [9, 9, 'haberdasher', 'Cem']])
     let result = tp.plan_route(0, 0, 'Cem')
     assert result == cons {data: [0, 0], next: cons {data: [1, 1], next: cons {data: [2, 2], next: cons {data: [3, 3], next: cons {data: [4, 4], next: cons {data: [5, 5], next: cons {data: [6, 6], next: cons {data: [7, 7], next: cons {data: [8, 8], next: cons {data: [9, 9], next: None}}}}}}}}}}
    
test "MST is not SSSP(route)":
    let tp = TripPlanner(
      [[-1.1, -1.1, 0, 0],
       [0, 0, 3, 0],
       [3, 0, 3, 3],
       [3, 3, 3, 4],
       [0, 0, 3, 4]],
      [[0, 0, 'food', 'Sandwiches'],
       [3, 0, 'bank', 'Union'],
       [3, 3, 'barber', 'Judy'],
       [3, 4, 'barber', 'Tony']])
    let result = tp.plan_route(-1.1, -1.1, 'Tony')
    assert result ==  cons {data: [-1.1, -1.1], next: cons {data: [0, 0], next: cons {data: [3, 4], next: None}}}
    
test "Destination is the 2nd of 3 POI at that location":
     let tp = TripPlanner(
      [[0, 0, 1.5, 0],
       [1.5, 0, 2.5, 0],
       [2.5, 0, 3, 0],
       [4, 0, 5, 0],
       [3, 0, 4, 0]],
      [[1.5, 0, 'bank', 'Union'],
       [3, 0, 'barber', 'Tony'],
       [5, 0, 'bar', 'Pasta'],
       [5, 0, 'barber', 'Judy'],
       [5, 0, 'food', 'Jollibee']])
     let result = tp.plan_route(0, 0, 'Judy')
     assert result == cons {data: [0, 0], next: cons {data: [1.5, 0], next: cons {data: [2.5, 0], next: cons {data: [3, 0], next: cons {data: [4, 0], next: cons {data: [5, 0], next: None}}}}}}
    
test "Two equvalent routes":
    let tp = TripPlanner(
      [[-2, 0, 0, 2],
       [0, 2, 2, 0],
       [2, 0, 0, -2],
       [0, -2, -2, 0]],
      [[2, 0, 'cooper', 'Dennis']])
    let result = tp.plan_route(-2, 0, 'Dennis')
    assert result == cons {data: [-2, 0], next: cons {data: [0, -2], next: cons {data: [2, 0], next: None}}}
    
test "BinHeap needs capacity":
    let tp = TripPlanner(
      [[0, 0, 0, 1],
       [0, 1, 3, 0],
       [0, 1, 4, 0],
       [0, 1, 5, 0],
       [0, 1, 6, 0],
       [0, 0, 1, 1],
       [1, 1, 3, 0],
       [1, 1, 4, 0],
       [1, 1, 5, 0],
       [1, 1, 6, 0],
       [0, 0, 2, 1],
       [2, 1, 3, 0],
       [2, 1, 4, 0],
       [2, 1, 5, 0],
       [2, 1, 6, 0]],
      [[0, 0, 'blacksmith', "Revere's Silver Shop"],
       [6, 0, 'church', 'Old North Church']])
    let result = tp.plan_route(0, 0, 'Old North Church')
    assert result ==  cons {data: [0, 0], next: cons {data: [2, 1], next: cons {data: [6, 0], next: None}}}
     
    
        
test "1 barber nearby":
    let tp = TripPlanner(
      [[0, 0, 1.5, 0],
       [1.5, 0, 2.5, 0],
       [2.5, 0, 3, 0]],
      [[1.5, 0, 'bank', 'Union'],
       [3, 0, 'barber', 'Tony']])
    let result = tp.find_nearby(0, 0, 'barber', 1)
    assert result == cons {data: [3, 0, 'barber', 'Tony'], next: None}
    

test "find bank from barber":
    let tp = TripPlanner(
      [[0, 0, 1.5, 0],
       [1.5, 0, 2.5, 0],
       [2.5, 0, 3, 0]],
      [[1.5, 0, 'bank', 'Union'],
       [3, 0, 'barber', 'Tony']])
    let result = tp.find_nearby(3, 0, 'bank', 1)
    assert result == cons {data: [1.5, 0, 'bank', 'Union'], next: None}
    
    
test " 2 relevant POI: one reachable":
    let tp = TripPlanner(
      [[0, 0, 1.5, 0],
       [1.5, 0, 2.5, 0],
       [2.5, 0, 3, 0],
       [4, 0, 5, 0]],
      [[1.5, 0, 'bank', 'Union'],
       [3, 0, 'barber', 'Tony'],
       [4, 0, 'food', 'Jollibee'],
       [5, 0, 'barber', 'Judy']])
    let result = tp.find_nearby(0, 0, 'barber', 2)
    assert result ==  cons {data: [3, 0, 'barber', 'Tony'], next: None}
    
test "  NO POIS in requested category":
    let tp = TripPlanner(
      [[0, 0, 1.5, 0],
       [1.5, 0, 2.5, 0],
       [2.5, 0, 3, 0],
       [4, 0, 5, 0]],
      [[1.5, 0, 'bank', 'Union'],
       [3, 0, 'barber', 'Tony'],
       [5, 0, 'barber', 'Judy']])
    let result = tp.find_nearby(0, 0, 'food', 1)
    assert result == None
    
test "Relevant POI isn't reachable":
    let tp = TripPlanner(
      [[0, 0, 1.5, 0],
       [1.5, 0, 2.5, 0],
       [2.5, 0, 3, 0],
       [4, 0, 5, 0]],
      [[1.5, 0, 'bank', 'Union'],
       [3, 0, 'barber', 'Tony'],
       [4, 0, 'food', 'Jollibee'],
       [5, 0, 'barber', 'Judy']])
    let result = tp.find_nearby(0, 0, 'food', 1)
    assert result == None 
    
    
test "BFS is not SSSP (nearby)":
    let tp = TripPlanner(
      [[0, 0, 0, 9],
       [0, 9, 9, 9],
       [0, 0, 1, 1],
       [1, 1, 2, 2],
       [2, 2, 3, 3],
       [3, 3, 4, 4],
       [4, 4, 5, 5],
       [5, 5, 6, 6],
       [6, 6, 7, 7],
       [7, 7, 8, 8],
       [8, 8, 9, 9]],
      [[7, 7, 'haberdasher', 'Archit'],
       [8, 8, 'haberdasher', 'Braden'],
       [9, 9, 'haberdasher', 'Cem']])
    let result = tp.find_nearby(0, 0, 'haberdasher', 2)
    assert result ==  cons {data: [8, 8, 'haberdasher', 'Braden'], next: cons {data: [7, 7, 'haberdasher', 'Archit'], next: None}}
    
test "MST is not SSSP (nearby)":
     let tp = TripPlanner(
      [[-1.1, -1.1, 0, 0],
       [0, 0, 3, 0],
       [3, 0, 3, 3],
       [3, 3, 3, 4],
       [0, 0, 3, 4]],
      [[0, 0, 'food', 'Sandwiches'],
       [3, 0, 'bank', 'Union'],
       [3, 3, 'barber', 'Judy'],
       [3, 4, 'barber', 'Tony']])
     let result = tp.find_nearby(-1.1, -1.1, 'barber', 1)
     assert result == cons {data: [3, 4, 'barber', 'Tony'], next: None}
     
test "2 relevant POIs; limit 3":
     let tp = TripPlanner(
      [[0, 0, 1.5, 0],
       [1.5, 0, 2.5, 0],
       [2.5, 0, 3, 0],
       [4, 0, 5, 0],
       [3, 0, 4, 0]],
      [[1.5, 0, 'bank', 'Union'],
       [3, 0, 'barber', 'Tony'],
       [4, 0, 'food', 'Jollibee'],
       [5, 0, 'barber', 'Judy']])
     let result = tp.find_nearby(0, 0, 'barber', 3)
     assert result == cons {data: [5, 0, 'barber', 'Judy'], next: cons {data: [3, 0, 'barber', 'Tony'], next: None}}
     
     
test "2 relevant equidistanct POIS; limit1":
    let tp = TripPlanner(
      [[-1, -1, 0, 0],
       [0, 0, 3.5, 0],
       [0, 0, 0, 3.5],
       [3.5, 0, 0, 3.5]],
      [[-1, -1, 'food', 'Jollibee'],
       [0, 0, 'bank', 'Union'],
       [3.5, 0, 'barber', 'Tony'],
       [0, 3.5, 'barber', 'Judy']])
    let result = tp.find_nearby(-1, -1, 'barber', 1)
    assert result ==  cons {data: [0, 3.5, 'barber', 'Judy'], next: None}
    
test "3 relevant POIS; father 2 at same location; limit 2":
    let tp = TripPlanner(
      [[0, 0, 1.5, 0],
       [1.5, 0, 2.5, 0],
       [2.5, 0, 3, 0],
       [4, 0, 5, 0],
       [3, 0, 4, 0]],
      [[1.5, 0, 'bank', 'Union'],
       [3, 0, 'barber', 'Tony'],
       [5, 0, 'barber', 'Judy'],
       [5, 0, 'barber', 'Lily']])
    let result = tp.find_nearby(0, 0, 'barber', 2)
    assert result == cons {data: [5, 0, 'barber', 'Lily'], next: cons {data: [3, 0, 'barber', 'Tony'], next: None}}
    
test "3 relevant POIs, farther 2 equidistan; limit 2":
    let tp = TripPlanner(
      [[0, 0, 1.5, 0],
       [1.5, 0, 2.5, 0],
       [2.5, 0, 3, 0],
       [4, 0, 5, 0],
       [3, 0, 4, 0]],
      [[1.5, 0, 'bank', 'Union'],
       [0, 0, 'barber', 'Lily'],
       [3, 0, 'barber', 'Tony'],
       [5, 0, 'barber', 'Judy']])
    let result = tp.find_nearby(2.5, 0, 'barber', 2)
    assert result == cons {data: [0, 0, 'barber', 'Lily'], next: cons {data: [3, 0, 'barber', 'Tony'], next: None}}
    
    
    
    
test "POI is 2nd of 3 in that location":
    let tp = TripPlanner(
      [[0, 0, 1.5, 0],
       [1.5, 0, 2.5, 0],
       [2.5, 0, 3, 0],
       [4, 0, 5, 0],
       [3, 0, 4, 0]],
      [[1.5, 0, 'bank', 'Union'],
       [3, 0, 'barber', 'Tony'],
       [5, 0, 'food', 'Jollibee'],
       [5, 0, 'barber', 'Judy'],
       [5, 0, 'bar', 'Pasta']])
    let result = tp.find_nearby(0, 0, 'barber', 2)
    assert result == cons {data: [5, 0, 'barber', 'Judy'], next: cons {data: [3, 0, 'barber', 'Tony'], next: None}}
    
    
test "at relevant location":
    let tp = TripPlanner(
    [[0, 0, 1, 1], 
    [0, 1.5, 2, 2], 
    [3, 4, 0, 0]], 
    [[0, 0, 'coffee', 'Lara'], 
    [0, 1.5, 'coffee', 'Elly'],
    [1, 1, 'coffee', 'Anna']])
    let result = tp.find_nearby(0, 0, 'coffee', 1)
    assert result ==  cons {data: [0, 0, 'coffee', 'Lara'], next: None} 
    
    

    
    
    
    
    
    
    
    
    
        
    
         
    
    

       
     
    
    
