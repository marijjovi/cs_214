#lang dssl2

# HW2: Stacks and Queues
let eight_principles = ["Know your rights.",
    "Acknowledge your sources.",
    "Protect your work.",
    "Avoid suspicion.",
    "Do your own work.",
    "Never falsify a record or permit another person to do so.",
    "Never fabricate data, citations, or experimental results.",
    "Always tell the truth when discussing your work with your instructor."]

import ring_buffer

interface STACK[T]:
    def push(self, element: T) -> NoneC
    def pop(self) -> T
    def empty?(self) -> bool?

# Defined in the `ring_buffer` library; copied here for reference.
# Do not uncomment! or you'll get errors.
# interface QUEUE[T]:
#     def enqueue(self, element: T) -> NoneC
#     def dequeue(self) -> T
#     def empty?(self) -> bool?

# Linked-list node struct (implementation detail):
struct _cons:
    let data
    let next: OrC(_cons?, NoneC)

###
### ListStack
###

class ListStack[T] (STACK):

    let head
    let tail 
    let length

    # Constructs an empty ListStack.
    def __init__ (self):
        self.head   = None 
        self.tail   = None 
        self.length = 0 
        
    def insert_front(self, data):
        self.head = _cons(data, self.head)
        if self.tail == None: self.tail = self.head
        
    def get_first(self):
        if _cons?(self.head): return self.head.data
        else: error("first of an empty list")
        
    def find_nth_node(self, n:int?):
        let val = self.head
        while not val == None:
            if n == 0:
                return val 
            else: 
                n = n - 1
                val = val.next
        error("list is too short")
        
    def get_nth(self, n:int?):
        return self.find_nth_node(n).data 
    
    def set_nth(self, n:int?, value):
        self.find_nth_node(n).data = value ## unsure abt the tail here, will revisit 
        
    def get_last(self): 
        if self.tail == None: error("last of an empty list")
        return self.tail.data 
        
        
    def push(self, element: T):
        if self.length == 0:
            self.head = _cons(element, None)
            self.length = 1 
            self.tail = self.head
        else:
            let new_tail = _cons(element, None)
            self.tail.next = new_tail
            self.tail = new_tail 
            self.length = self.length + 1 
        
        
    def pop(self): 
        if self.length == 0: error("there are no elements in this list")
        else:
            let old_tail = self.tail.data
            self.length = self.length - 1
            if self.length == 0: 
                self.head = None 
                self.tail = None
                return old_tail ## does this return an empty list or [None, None] value ? 
            else:  
                self.tail = self.find_nth_node(self.length - 1)
                self.tail.next = None 
                return old_tail
            
    def empty?(self):
        if self.length == 0: return True 
        else:
            return False 
            
            
             
            
        
            

    # Other methods you may need can go here.

test "woefully insufficient":
    let s = ListStack()
    s.push(2)
    assert s.pop() == 2
    
test "one_two":
    let new = ListStack()
    assert_error new.pop() == "there are no elements in this list"
    assert_error new.push()
    new.push(3)
    new.push(5)
    assert new.pop() == 5
    assert new.pop() == 3
    assert new.empty?()
    assert_error new.pop() == "there are no elements in this list"
    new.push("string")
    new.push(34)
    assert not new.empty?()
    new.push(23)
    new.push(12)
    new.push(12)
    new.pop()
    new.pop()
    new.pop()
    assert new.pop() == 34
    
    
    
    
    
    
    
    

###
### ListQueue
###

class ListQueue[T] (QUEUE):

    let head 
    let tail 
    let length

    # Constructs an empty ListQueue.
    def __init__ (self):
        self.head   = None 
        self.tail   = None 
        self.length = 0 

    def enqueue(self, element: T): 
        if self.length == 0:
            self.head = _cons(element, None)
            self.length = 1 
            self.tail = self.head
        else: 
            let new_tail = _cons(element, None)
            self.tail.next = new_tail
            self.tail = new_tail ## unsure if this copies or what, check later jic
            self.length = self.length + 1 
        
            
    def dequeue(self):
        if self.length == 0: error("there is nothing to dequeue in this list")
        else: 
            let dequeued_song = self.head.data
            self.length = self.length - 1 
            if self.length == 0:
                self.head = None
                self.tail = None
                return dequeued_song ## same as above 
            else:
                self.head = self.head.next 
                return dequeued_song
                
                
           
        
    def empty?(self):
        if self.length == 0: return True 
        else:
            return False 
        
    # Other methods you may need can go here.

test "woefully insufficient, part 2":
    let q = ListQueue()
    q.enqueue(2)
    assert q.dequeue() == 2
    
test "three four":
    let little_man = ListQueue()
    assert_error little_man.dequeue() == "there is nothing to dequeue in this list"
    little_man.enqueue(4)
    assert not little_man.empty?()
    little_man.enqueue(31)
    little_man.enqueue(45)
    assert little_man.dequeue() == 4
    assert little_man.dequeue() == 31
    assert little_man.dequeue() == 45 
    assert little_man.empty?()
    assert_error little_man.dequeue() == "there is nothing to dequeue in this list"
    
    

###
### Playlists
###

struct song:
   let title: str?
   let artist: str?
   let album: str?

# Enqueue five songs of your choice to the given queue, then return the first
# song that should play.
def fill_playlist (q: QUEUE!):
    q.enqueue(song("A U Meduvremenu","Goran i OK band", "2"))
    q.enqueue(song("Slatka Mala", "Jelena Karleusa", "The Diamond Collection"))
    q.enqueue(song("Tri case", "Milica Todorovic", "Letnji Mix Hitova 3"))
    q.enqueue(song("Kafana na Balkanu", "Funky G", "Kafana na Balkanu"))
    q.enqueue(song("Ruzmarin", "Sasa Matic", "Andeo cuvar"))
    q.dequeue()

test "ListQueue playlist":
    let playlist_time = ListQueue()
    assert fill_playlist(playlist_time) == song("A U Meduvremenu","Goran i OK band", "2")
    assert fill_playlist(playlist_time) == song("Slatka Mala", "Jelena Karleusa", "The Diamond Collection")
    assert fill_playlist(playlist_time) == song("Tri case", "Milica Todorovic", "Letnji Mix Hitova 3")

# To construct a RingBuffer: RingBuffer(capacity)
test "RingBuffer playlist":
    let d = RingBuffer(5)
    for i in range(5): d.enqueue(i)
    assert d.full?()
    assert d.dequeue() == 0
    assert d.dequeue() == 1
    assert d.dequeue() == 2
    d.enqueue(5)
    d.enqueue(6)
    d.enqueue(7)
    assert d.full?()
    assert d.dequeue() == 3
    assert d.dequeue() == 4
    assert d.dequeue() == 5
    assert d.dequeue() == 6
    assert d.dequeue() == 7
    assert not d.full?()

