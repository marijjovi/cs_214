#lang dssl2

# HW3: Dictionaries

import sbox_hash
let eight_principles = ["Know your rights.",
"Acknowledge your sources.",
"Protect your work.",
"Avoid suspicion.",
"Do your own work.",
"Never falsify a record or permit another person to do so.",
"Never fabricate data, citations, or experimental results.",
"Always tell the truth when discussing your work with your instructor."]
# A signature for the dictionary ADT. The contract parameters `K` and
# `V` are the key and value types of the dictionary, respectively.
interface DICT[K, V]:
    # Returns the number of key-value pairs in the dictionary.
    def len(self) -> nat?
    # Is the given key mapped by the dictionary?
    # Notation: `key` is the name of the parameter. `K` is its contract.
    def mem?(self, key: K) -> bool?
    # Gets the value associated with the given key; calls `error` if the
    # key is not present.
    def get(self, key: K) -> V
    # Modifies the dictionary to associate the given key and value. If the
    # key already exists, its value is replaced.
    def put(self, key: K, value: V) -> NoneC
    # Modifes the dictionary by deleting the association of the given key.
    def del(self, key: K) -> NoneC
    # The following method allows dictionaries to be printed
    def __print__(self, print)

struct _cons:
    let key
    let value
    let next
    

class AssociationList[K, V] (DICT):

    let _head
    let _tail 
    let _length

    def __init__(self):
        self._head = None 
        self._tail = None
        self._length = 0 
        
    def len(self):
        return self._length 
        
    def helper_return_node(self, key):
        let starter = self._head
        let i = 0 
        while i < self._length:
            if starter.key == key: return starter
            if starter.next == None: return False
            else:
                starter = starter.next
        return False
        
    def mem?(self, key):
       if self._length == 0: return False
       let return_val  = self.helper_return_node(key)
       if return_val == False: return False 
       else: return True
       
    def get(self, key):
        if self._length == 0: error  
        let return_val = self.helper_return_node(key)
        if return_val == False: error ("Key not present in dictionary")
        else: 
            return return_val.value
            
    def put(self, key, value):
        if self._length == 0:
            self._head = _cons(key, value, None)
            self._tail = self._head
            self._length = 1 
        else:
           let return_val = self.helper_return_node(key)
           if return_val == False: 
               let new_tail = _cons(key, value, None)
               self._tail.next = new_tail
               self._tail = new_tail
               self._length = self._length + 1
           else:
              return_val.value = value
               
               
    def circling_through(self, key):
         let starter = self._head 
         let returner = self._head.next  
         let i = 0 
         while i < self._length:
             if returner.key == key: return starter
             else:
                 starter = starter.next
                 returner = returner.next           
               
    def del(self, key):
        if self._length == 0: return False
        if self._length == 1:
            if key == self._head:
                self._head = None
                self._tail = None
                self._length = 0 
            else: return False
        else: 
           let deleted_value = self.helper_return_node(key)
           if deleted_value == False: return False
           elif self._head.key == deleted_value.key:
               self._head = deleted_value.next
               self._length = self._length - 1
           elif deleted_value != self._head: 
               let prev_value = self.circling_through(key)
               if deleted_value == self._tail:
                   self._tail = prev_value
               prev_value.next = deleted_value.next 
               self._length = self._length - 1 
            
           
        

    # See above.
    def __print__(self, print):
        print("#<object:AssociationList head=%p>", self._head)

    # Other methods you may need can go here.


test 'yOu nEeD MorE tEsTs':
    let a = AssociationList()
    assert not a.mem?('hello')
    a.put('hello', 5)
    assert a.len() == 1
    assert a.mem?('hello')
    assert a.get('hello') == 5
    
test 'error cases':
    let e = AssociationList()
    assert not e.mem?("true")
    ##assert_error e.del("seven") 
    assert_error e.get("wow") 
    
test "messing around":
    let m = AssociationList()
    m.put("real", 4)
    m.put("dog", 234)
    m.put("bow wow pow", 31)
    m.put("lover", 21)
    m.put("goofy", 211)
    m.put("doctor", 2)
    assert m.len() == 6
    assert m.get("bow wow pow") == 31
    m.del("lover")
    m.del("goofy")
    ##m.__print__(print)
    m.put("length", 41) 
    assert m.len() == 5
    assert_error m.get("lover") 
    m.put("real" , 234)
    m.get("real") == 234
    
test "Checkin in":
    let o = AssociationList()
    o.put(5, 'five')
    o.put(15, 'fifteen')
    o.put(25, 'twenty-five')
    ##o.__print__(print)
    o.del(5)
    #o.__print__(print)
    assert o.mem?(5) is False
    assert o.mem?(15) is True
    assert o.mem?(25) is True
    
test "Work please":
    let o = AssociationList()
    o.put(5, 'five')
    o.del(6)
    assert o.get(5) == 'five'
    assert o.len() == 1


class HashTable[K, V] (DICT):
    let _hash
    let _size
    let _data

    def __init__(self, nbuckets: nat?, hash: FunC[AnyC, nat?]):
        self._hash = hash
        self._size = 0 
        self._data = [None; max(0, nbuckets)]
        for i in range(len(self._data)):
            self._data[i] = AssociationList()
        
    #   ^ YOUR WORK GOES HERE

    # This avoids trying to print the hash function, since it's not really
    # printable and isn’t useful to see anyway:
    def __print__(self, print):
        print("#<object:HashTable  _hash=... _size=%p _data=%p>",
              self._size, self._data)

    # Other methods you may need can go here.
    def len(self):
        return self._size
    
    def bucket_reveal(self, K):
        let start = self._hash(K)
        let maybe_key = start % self._data.len()
        let target_bucket = self._data[maybe_key]
        return target_bucket
    
    def mem?(self, K):
        if self._size == 0:
            return False
        let target_bucket = self.bucket_reveal(K)
        let response = target_bucket.mem?(K)
        return response
        
    def get(self, K):
        if self._size == 0: error ("Empty hash table")
        let target_bucket = self.bucket_reveal(K)
        let here_or_nah = target_bucket.mem?(K)
        if here_or_nah == False: error ("Key not present")
        let val = target_bucket.get(K)
        return val 
        
   
    def put(self, K, V):
        let target_bucket = self.bucket_reveal(K)
        let here_or_nah = target_bucket.mem?(K)
        if here_or_nah == False:
            self._size = self._size + 1
        target_bucket.put(K, V)
        
        
    def del(self, K):
        if self._size == 0: error ("Nothing to delete")
        let target_bucket = self.bucket_reveal(K)
        let here_or_nah = target_bucket.mem?(K)
        if here_or_nah == False:
            return 
        else:
            target_bucket.del(K)
            self._size = self._size - 1
        


# first_char_hasher(String) -> Natural
# A simple and bad hash function that just returns the ASCII code
# of the first character.
# Useful for debugging because it's easily predictable.
def first_char_hasher(s: str?) -> int?:
    if s.len() == 0:
        return 0
    else:
        return int(s[0])
        
test "bad_hasher":
    let h = HashTable(10, first_char_hasher)
    h.put("apple", 75)
    h.put("window", 33)
    h.put("car", 12)
    assert h.len() == 3
    assert h.mem?("apple")
    h.put("altruist", 23)
    h.get("apple") == 75
    assert h.len() == 4
    h.get("altruist") == 23
    h.put("apple", 192013)
    h.get("apple") == 192013
    assert h.len() == 4
    



test 'yOu nEeD MorE tEsTs, part 2':
    let h = HashTable(10, make_sbox_hash())
    assert not h.mem?('hello')
    h.put('hello', 5)
    assert h.len() == 1
   ## h.__print__(print)
    assert h.mem?('hello')
    assert h.get('hello') == 5

test "data types are really fun actually":
    let hashy = HashTable(15, make_sbox_hash())
    assert_error hashy.del("weeh") 
    hashy.put("girls", 2345)
    hashy.put("boys", 212)
    hashy.put("tracks", 54)
    assert_error hashy.get("massive") 
    hashy.put("rod", 99)
    hashy.put("freakish", 943134)
    assert hashy.len() == 5 
    ##hashy.__print__(print)
    hashy.get("tracks") == 54
    hashy.mem?("rod") == 99
    hashy.del("girls")
    hashy.del("boys")
    hashy.del("tracks")
    hashy.del("rod")
    assert hashy.len() == 1
    hashy.__print__(print)
    hashy.del("freakish")
    assert hashy.len() == 0
    
test "deletion":
    let o = HashTable(10, int)
    o.put(5, 'five')
    o.del(5)
    assert o.mem?(5) is False
    
test "deletion2":
    let o = HashTable(10, int)
    o.put(5, 'five')
    o.put(15, 'fifteen')
    o.put(25, 'twenty-five')
    o.del(5)
    assert o.mem?(5) is False
    assert o.mem?(15) is True
    assert o.mem?(25) is True

test "dele_3":
    let o = HashTable(10, int)
    o.put(5, 'five')
    o.put(15, 'fifteen')
    o.put(25, 'twenty-five')
    o.del(15)
    assert o.mem?(5) is True
    assert o.mem?(15) is False
    assert o.mem?(25) is True

test "four_four_four":
    let o = HashTable(10, int)
    o.put(5, 'five')
    o.put(15, 'fifteen')
    o.put(25, 'twenty-five')
    o.del(25)
    assert o.mem?(5) is True
    assert o.mem?(15) is True
    assert o.mem?(25) is False

test "fiviosss":
    let o = HashTable(10, int)
    o.put(3, 'three')
    assert o.get(3) == 'three'
    o.del(3)
    o.put(5, 'five')
    assert o.get(5) == 'five'
    o.del(5)
    o.put(7, 'seven')
    assert o.get(7) == 'seven'
    o.del(7)
    assert o.len() == 0 
    assert_error o.get(3)
    assert_error o.get(5)
    assert_error o.get(7)
    
test "almost done":
    let o = HashTable(10, int)
    o.put(5, 'five')
    o.put(5, 'bees')
    o.del(5)
    assert o.mem?(5) is False

test "last Failure":
    let o = HashTable(10, int)
    o.put(5, 'five')
    o.del(6)
    assert o.len() == 1

struct phrase_holder: 
    let english
    let pronounciation 
    
def compose_phrasebook(d: DICT!) -> DICT?:
    d.put("Krava", phrase_holder("Cow", "krAH-vuh"))
    d.put("Dodji" , phrase_holder("Come", "Doh-G"))
    d.put("Vatra" , phrase_holder("Fire", "va-tra"))
    d.put("Krv", phrase_holder("Blood", "kur-vuh"))
    d.put("Anketa", phrase_holder("Survey", "an-keh-tah"))
    return d 
#   ^ YOUR WORK GOES HERE

test "AssociationList phrasebook":
    let Whoopie = AssociationList()
    let new_whoopie = compose_phrasebook(Whoopie)
    assert new_whoopie.get("Vatra").pronounciation == "va-tra"

test "HashTable phrasebook":
    let BabyGirl = HashTable(7, make_sbox_hash())
    let new_babygirl = compose_phrasebook(BabyGirl)
    assert new_babygirl.get("Dodji").pronounciation == "Doh-G"