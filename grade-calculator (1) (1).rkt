#lang dssl2

let eight_principles = ["Know your rights.",
                        "Acknowledge your sources.",
                        "Protect your work.",
                         "Avoid suspicion.",
                         "Do your own work.",
                        "Never falsify a record or permit another person to do so.",
                         "Never fabricate data, citations, or experimental results.",
                        "Always tell the truth when discussing your work with your instructor."]


# HW1: Grade Calculator

###
### Data Definitions
###

let outcome? = OrC("got it", "almost there", "on the way", "not yet",
                   "missing honor code", "cannot assess")

struct homework:
    let outcome: outcome?
    let self_eval_score: nat?

struct project:
    let outcome: outcome?
    let docs_modifier: int?

let letter_grades = ["F", "D", "C-", "C", "C+", "B-", "B", "B+", "A-", "A"]
def letter_grade? (str):
    let found? = False
    for g in letter_grades:
        if g == str: found? = True
    return found?


###
### Modifiers
###

def worksheets_modifier (worksheet_percentages: TupC[num?, num?]) -> int?:
    
    if worksheet_percentages.len() > 2: error('worksheet_modifier: not properly formatted')
    if worksheet_percentages[0] == 1.0 and worksheet_percentages[1] == 1.0: return 1  
    if worksheet_percentages[0] >= 0.8 and worksheet_percentages[1] >= 0.8: return 0
    else: return -1 
    
    

def exams_modifiers (exam1: nat?, exam2: nat?) -> int?:

    let exam1_mod = -3
    let exam2_mod = -3
    let additional_mod = 0 
    if 4 <= exam1: 
        exam1_mod = -2
    if 8 <= exam1: 
        exam1_mod = -1
    if 11 <= exam1: 
        exam1_mod = 0 
    if 17 <= exam1: 
        exam1_mod = 1 
    if 4 <= exam2: 
        exam2_mod = -2
    if 8 <= exam2: 
        exam2_mod = -1
    if 11 <= exam2: 
        exam2_mod = 0 
    if 17 <= exam2: 
        exam2_mod = 1 
    if (exam2_mod - exam1_mod) >= 2: 
        additional_mod = 1
    let full_total = exam1_mod + exam2_mod + additional_mod
    return full_total
        
    
def self_evals_modifier (hws: VecC[homework?]) -> int?:
    if hws.len() != 5: error('self_evals_modifier: wrong number of hws')
    let scored_five = 0 
    let scored_three = 0 
    let scored_two = 0 
    for hw in hws: 
       let val_score = hw.self_eval_score
       if val_score == 5: 
           scored_five = scored_five + 1
           scored_three = scored_three + 1
       elif val_score >= 3: 
           scored_three = scored_three + 1 
       elif val_score <= 2: 
           scored_two = scored_two + 1

       
    if scored_five >= 4: return 1
    if scored_three >= 3: return 0 
    if scored_two >= 3: return -1  
    
    
def in_class_modifier (scores: VecC[nat?]) -> int?:
    # 'scores' vector represenst the scores received 
    # on all in-class exercises conducted in class, 
    # including 0s due to missed classes. 
    # The length of the vector should be the number of 
    # exercises conducted in class.
    if scores.len() == 0: error('in_class_modifier: received empty vector')
    let total_ones = 0
    for score in scores:
        total_ones = total_ones + score
    # computes percentage and then rounds
    let in_class_percentage = (total_ones/scores.len())*100
    let in_class_percentage_rounded = (in_class_percentage + 0.5).floor()
    if in_class_percentage_rounded >= 90: return 1
    if in_class_percentage_rounded >= 50 and in_class_percentage_rounded < 90: return 0
    return -1

###
### Letter Grade Helpers
###

# Is outcome x enough to count as outcome y?
def is_at_least (x:outcome?, y:outcome?) -> bool?:
    if x == "got it": return True
    if x == "almost there" \
        and (y == "almost there" or y == "on the way" or y == "not yet"):
        return True
    if x == "on the way" and (y == "on the way" or y == "not yet"): return True
    return False

def apply_modifiers (base_grade: letter_grade?, total_modifiers: int?) -> letter_grade?:
    #"F" - 0, "D" - 1, "C-" - 2, "C"- 3, "C+" - 4 , "B-"- 5 , "B" - 6, "B+" - 7, "A-" - 8, "A" - 9
    let starting_score = 0 
    if base_grade == "A-":
        starting_score = 8 
    if base_grade == "B":
         starting_score = 6
    if base_grade ==  "C+":
        starting_score = 4 
    if base_grade == "D":
        starting_score = 1 
    if base_grade == "F": return "F"
    let final_grade = starting_score + total_modifiers 
    if final_grade >= 9: return "A"
    if final_grade == 8: return "A-"
    if final_grade == 7: return "B+"
    if final_grade == 6: return "B"
    if final_grade == 5: return "B-"
    if final_grade == 4: return "C+"
    if final_grade == 3: return "C"
    if final_grade == 2: return "C-"
    if final_grade == 1: return "D"
    if final_grade <= 0: return "F"


###
### Students
###

class Student:
    let name: str?
    let homeworks: TupC[homework?, homework?, homework?, homework?, homework?]
    let project: project?
    let worksheet_percentages: TupC[num?, num?]
    let in_class_scores: VecC[nat?]
    let exam_scores: TupC[nat?, nat?]

    def __init__ (self, name, homeworks, project, worksheet_percentages, in_class_scores, exam_scores):
        self.name = name 
        self.homeworks = homeworks 
        self.project = project 
        self.worksheet_percentages = worksheet_percentages
        self.in_class_scores = in_class_scores
        self.exam_scores = exam_scores

    def get_homework_outcomes(self) -> VecC[outcome?]:
        let homeworks_new = self.homeworks
        let outcomes = [hw.outcome for hw in  homeworks_new]
        return outcomes 
           
        

    def get_project_outcome(self) -> outcome?:
        let p_outcome = self.project
        return p_outcome.outcome

    def resubmit_homework (self, n: nat?, new_outcome: outcome?) -> NoneC:
        if n > 5 or n < 0 : error("resubmit_homework: homework not found") 
        let that_homework = self.homeworks[n - 1]
        that_homework.outcome = new_outcome
       
       

    def resubmit_project (self, new_outcome: outcome?) -> NoneC:
        self.project.outcome = new_outcome

    def base_grade (self) -> letter_grade?:
        let n_got_its       = 0
        let n_almost_theres = 0
        let n_on_the_ways   = 0
        for o in self.get_homework_outcomes():
            if is_at_least(o, "got it"):
                n_got_its       = n_got_its       + 1
            if is_at_least(o, "almost there"):
                n_almost_theres = n_almost_theres + 1
            if is_at_least(o, "on the way"):
                n_on_the_ways   = n_on_the_ways   + 1
        let project_outcome = self.get_project_outcome()
        if n_got_its == 5 and project_outcome == "got it": return "A-"
        # the 4 "almost there"s or better include the 3 "got it"s
        if n_got_its >= 3 and n_almost_theres >= 4 and n_on_the_ways >= 5 \
           and is_at_least(project_outcome, "almost there"):
            return "B"
        if n_got_its >= 2 and n_almost_theres >= 3 and n_on_the_ways >= 4 \
           and is_at_least(project_outcome, "on the way"):
            return "C+"
        if n_got_its >= 1 and n_almost_theres >= 2 and n_on_the_ways >= 3 \
           and is_at_least(project_outcome, "on the way"):
            return "D"
        return "F"

    def project_above_expectations_modifier (self) -> int?:
        let base_grade = self.base_grade()
        if base_grade == 'A-': return 0 # expectations are already "got it"
        if base_grade == 'B':
            if is_at_least(self.project.outcome, 'got it'):       return 1
            else: return 0
        else:
            # two steps ahead of expectations
            if is_at_least(self.project.outcome, 'got it'):       return 2
            # one step ahead of expectations
            if is_at_least(self.project.outcome, 'almost there'): return 1
            else: return 0

    def total_modifiers (self) -> int?:
        let wkst_mod = worksheets_modifier(self.worksheet_percentages)
        let exam_mod = exams_modifiers(self.exam_scores[0], self.exam_scores[1] )
        let hw_mod = self_evals_modifier(self.homeworks)
        let in_class_mod = in_class_modifier(self.in_class_scores)
        let score = wkst_mod + exam_mod + hw_mod + in_class_mod + self.project.docs_modifier + self.project_above_expectations_modifier()
        return score
        
    def letter_grade (self) -> letter_grade?:
        let tots = self.base_grade()
        let nots =  self.total_modifiers()
        return apply_modifiers(tots, nots)

###
### Feeble attempt at a test suite
###
        
test 'worksheets_modifier, negative modifier':
    assert worksheets_modifier([0.5, 0.85]) == -1
    assert worksheets_modifier([0.3, 0.79]) == -1
    
test 'worksheets_modifier, testing codes':
    assert worksheets_modifier([0.8, 0.85]) == 0 
    assert worksheets_modifier([1.0, 1.0]) == 1 
    assert worksheets_modifier([1.0, 0.3]) == -1
    assert worksheets_modifier([0.79, 0.6]) == -1
    assert worksheets_modifier([0.79, 1.0]) == -1
    assert worksheets_modifier([1.0, 0.6]) == -1
    assert_error worksheets_modifier([1.0, 1.0, 0.8]) == 'worksheet_modifier: not properly formatted'
    
test "exams_modifiers, testing codes":
    assert exams_modifiers(20, 20) == 2
    assert exams_modifiers(4, 8) == -3 
    assert exams_modifiers(4, 17) == 0
    
test "apply_modifiers":
    assert apply_modifiers("A-", 3)  == "A"
    assert apply_modifiers("C+", 3)  == "B+"
    assert apply_modifiers("D", -4)  == "F"
    assert apply_modifiers('F', +1) == 'F'
    assert apply_modifiers('F', +3) == 'F'
    
    
    
    

test "self_evals_modifier, testing codes":   
    let fake_scoring_fail = [homework("got it", 5),
                     homework("got it", 5),
                     homework("got it", 5),
                     homework("got it", 5),
                     homework("got it", 5), 
                     homework("got it", 5)]
                    
    let fake_scoring_1 = [homework("got it", 4),
                     homework("got it", 5),
                     homework("got it", 5),
                     homework("got it", 5),
                     homework("got it", 5)]   
                     
    let fake_scoring_2 = [homework("got it", 3),
                     homework("got it", 3),
                     homework("got it", 3),
                     homework("got it", 5),
                     homework("got it", 1)] #0 
                     
    let fake_scoring_3 = [homework("got it", 3),
                     homework("got it", 3),
                     homework("got it", 2),
                     homework("got it", 2),
                     homework("got it", 2)] #-1
                     
    let fake_scoring_4 = [homework("got it", 4),
                     homework("got it", 4),
                     homework("got it", 5),
                     homework("got it", 5),
                     homework("got it", 5)]
                     
    let fake_scoring_5 = [homework("got it", 4),
                     homework("got it", 5),
                     homework("got it", 5),
                     homework("got it", 5),
                     homework("got it", 4)]
    
    let fake_scoring_6 = [homework("got it", 4),
                     homework("got it", 5),
                     homework("got it", 0),
                     homework("got it", 5),
                     homework("got it", 4)]
                     
    let fake_scoring_7 = [homework("got it", 4),
                     homework("got it", 5),
                     homework("got it", 0),
                     homework("got it", 0),
                     homework("got it", 4)]      
                                
    let fake_scoring_8 = [homework("got it", 5),
                     homework("got it", 0),
                     homework("got it", 3),
                     homework("got it", 0),
                     homework("got it", 3)]
                     
    assert_error self_evals_modifier (fake_scoring_fail) == 'self_evals_modifier: wrong number of hws'
    assert self_evals_modifier (fake_scoring_1) == 1
    assert self_evals_modifier (fake_scoring_2) == 0 
    assert self_evals_modifier (fake_scoring_3) == -1 
    assert self_evals_modifier (fake_scoring_4) == 0 
    assert self_evals_modifier (fake_scoring_5) == 0 
    assert self_evals_modifier (fake_scoring_6) == 0 
    assert self_evals_modifier (fake_scoring_7) == 0 
    assert self_evals_modifier (fake_scoring_8) == 0 
    
    

test 'Student#1':
    let maki = Student("Cutie", 
                    [homework("got it", 5),
                     homework("got it", 5),
                     homework("got it", 5),
                     homework("got it", 5),
                     homework("got it", 5)], 
                     project("not yet", 1), 
                      [1.0, 1.0],
                    [1, 1, 1, 1, 1], # Since vector length can't be 0
                    [20, 20])
    assert maki.get_homework_outcomes() == ["got it", "got it","got it","got it","got it"]
    assert maki.get_project_outcome() == "not yet"
    maki.resubmit_homework (4, "on the way") #ran a print statement here
    maki.resubmit_project("almost there") #ran a print statement here as well 
    assert maki.total_modifiers() == 6
    assert_error maki.resubmit_homework (7, "on the way") == "resubmit_homework: homework not found"
    assert_error maki.resubmit_homework (0, "on the way") == "resubmit_homework: homework not found"
                           
    
    
 
test 'Student#letter_grade, worst case scenario':
    let s = Student('Everyone, right now',
                    [homework("not yet", 0),
                     homework("not yet", 0),
                     homework("not yet", 0),
                     homework("not yet", 0),
                     homework("not yet", 0)],
                    project("not yet", -1),
                    [0.0, 0.0],
                    [0], # Since vector length can't be 0
                    [0, 0])
    assert s.base_grade() == 'F'
    assert s.total_modifiers() == -10
    assert s.letter_grade() == 'F'
    

test 'meow':
    let s = Student("YIPEEE",
                    [homework("got it", 3),
                     homework("almost there", 3),
                     homework("on the way", 3),
                     homework("not yet", 2),
                     homework("not yet", 2)],
                    project("almost there", 1),
                    [1.0, 1.0],
                    [0, 0, 1, 1, 1],
                    [8, 17])
    assert s.base_grade() == 'D'
    assert s.total_modifiers() == 4
    assert s.letter_grade() == 'B-'                                                                             


test 'Student#letter_grade, best case scenario':
    let s = Student("You, if you work harder than you've ever worked",
                    [homework("got it", 5),
                     homework("got it", 5),
                     homework("got it", 5),
                     homework("got it", 5),
                     homework("got it", 5)],
                    project("got it", 1),
                    [1.0, 1.0],
                    [1, 1, 1, 1, 1],
                    [20, 20])
    assert s.base_grade() == 'A-'
    assert s.total_modifiers() == 6
    assert s.letter_grade() == 'A'                