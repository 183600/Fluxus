import json
import pickle
import csv
from datetime import datetime, timedelta
from typing import List, Dict, Optional, Any, Union
from dataclasses import dataclass, asdict
from enum import Enum
import hashlib
import uuid

class GradeType(Enum):
    HOMEWORK = "homework"
    QUIZ = "quiz"
    MIDTERM = "midterm"
    FINAL = "final"
    PROJECT = "project"
    PARTICIPATION = "participation"

@dataclass
class Grade:
    """Represents a single grade entry."""
    id: str
    student_id: str
    subject: str
    grade_type: GradeType
    points_earned: float
    points_possible: float
    date_assigned: datetime
    date_submitted: Optional[datetime] = None
    comments: str = ""
    
    def __post_init__(self):
        if self.points_earned < 0 or self.points_possible <= 0:
            raise ValueError("Invalid points values")
        if self.points_earned > self.points_possible:
            raise ValueError("Points earned cannot exceed points possible")
    
    @property
    def percentage(self) -> float:
        """Calculate percentage score."""
        return (self.points_earned / self.points_possible) * 100
    
    @property
    def letter_grade(self) -> str:
        """Convert percentage to letter grade."""
        percentage = self.percentage
        if percentage >= 97: return "A+"
        elif percentage >= 93: return "A"
        elif percentage >= 90: return "A-"
        elif percentage >= 87: return "B+"
        elif percentage >= 83: return "B"
        elif percentage >= 80: return "B-"
        elif percentage >= 77: return "C+"
        elif percentage >= 73: return "C"
        elif percentage >= 70: return "C-"
        elif percentage >= 67: return "D+"
        elif percentage >= 63: return "D"
        elif percentage >= 60: return "D-"
        else: return "F"
    
    def is_late(self) -> bool:
        """Check if assignment was submitted late."""
        if not self.date_submitted:
            return False
        return self.date_submitted > self.date_assigned

@dataclass
class Student:
    """Represents a student with personal information and academic data."""
    id: str
    first_name: str
    last_name: str
    email: str
    phone: str
    date_of_birth: datetime
    enrollment_date: datetime
    major: str
    gpa: float = 0.0
    credits_completed: int = 0
    is_active: bool = True
    
    def __post_init__(self):
        if not self.email or "@" not in self.email:
            raise ValueError("Invalid email address")
        if self.gpa < 0.0 or self.gpa > 4.0:
            raise ValueError("GPA must be between 0.0 and 4.0")
    
    @property
    def full_name(self) -> str:
        """Get full name."""
        return f"{self.first_name} {self.last_name}"
    
    @property
    def age(self) -> int:
        """Calculate current age."""
        today = datetime.now()
        return today.year - self.date_of_birth.year - (
            (today.month, today.day) < (self.date_of_birth.month, self.date_of_birth.day)
        )
    
    def update_gpa(self, new_gpa: float) -> None:
        """Update student's GPA."""
        if not (0.0 <= new_gpa <= 4.0):
            raise ValueError("GPA must be between 0.0 and 4.0")
        self.gpa = new_gpa

class StudentManager:
    """Comprehensive student management system."""
    
    def __init__(self):
        self.students: Dict[str, Student] = {}
        self.grades: Dict[str, List[Grade]] = {}  # student_id -> grades
        self.courses: Dict[str, Dict[str, Any]] = {}
        self._grade_weights = {
            GradeType.HOMEWORK: 0.20,
            GradeType.QUIZ: 0.15,
            GradeType.MIDTERM: 0.25,
            GradeType.FINAL: 0.30,
            GradeType.PROJECT: 0.10,
            GradeType.PARTICIPATION: 0.0
        }
    
    def add_student(self, student: Student) -> bool:
        """Add a new student to the system."""
        if student.id in self.students:
            return False
        
        self.students[student.id] = student
        self.grades[student.id] = []
        return True
    
    def remove_student(self, student_id: str) -> bool:
        """Remove a student from the system."""
        if student_id not in self.students:
            return False
        
        del self.students[student_id]
        del self.grades[student_id]
        return True
    
    def get_student(self, student_id: str) -> Optional[Student]:
        """Get student by ID."""
        return self.students.get(student_id)
    
    def search_students(self, query: str) -> List[Student]:
        """Search students by name, email, or major."""
        query = query.lower()
        results = []
        
        for student in self.students.values():
            if (query in student.first_name.lower() or 
                query in student.last_name.lower() or
                query in student.email.lower() or
                query in student.major.lower()):
                results.append(student)
        
        return results
    
    def add_grade(self, grade: Grade) -> bool:
        """Add a grade for a student."""
        if grade.student_id not in self.students:
            return False
        
        self.grades[grade.student_id].append(grade)
        return True
    
    def get_student_grades(self, student_id: str, subject: Optional[str] = None) -> List[Grade]:
        """Get all grades for a student, optionally filtered by subject."""
        if student_id not in self.grades:
            return []
        
        grades = self.grades[student_id]
        if subject:
            grades = [g for g in grades if g.subject.lower() == subject.lower()]
        
        return grades
    
    def calculate_grade_average(self, student_id: str, subject: Optional[str] = None,
                              grade_type: Optional[GradeType] = None) -> Optional[float]:
        """Calculate average grade for a student."""
        grades = self.get_student_grades(student_id, subject)
        
        if grade_type:
            grades = [g for g in grades if g.grade_type == grade_type]
        
        if not grades:
            return None
        
        total_points = sum(g.points_earned for g in grades)
        total_possible = sum(g.points_possible for g in grades)
        
        return (total_points / total_possible) * 100 if total_possible > 0 else None
    
    def calculate_weighted_grade(self, student_id: str, subject: str) -> Optional[float]:
        """Calculate weighted grade for a subject."""
        grades = self.get_student_grades(student_id, subject)
        if not grades:
            return None
        
        weighted_score = 0.0
        total_weight = 0.0
        
        for grade_type, weight in self._grade_weights.items():
            type_grades = [g for g in grades if g.grade_type == grade_type]
            if type_grades:
                avg = sum(g.percentage for g in type_grades) / len(type_grades)
                weighted_score += avg * weight
                total_weight += weight
        
        return weighted_score / total_weight if total_weight > 0 else None
    
    def get_class_statistics(self, subject: str) -> Dict[str, Any]:
        """Get statistics for a specific class/subject."""
        all_grades = []
        for student_grades in self.grades.values():
            subject_grades = [g for g in student_grades if g.subject.lower() == subject.lower()]
            all_grades.extend(subject_grades)
        
        if not all_grades:
            return {"error": "No grades found for this subject"}
        
        percentages = [g.percentage for g in all_grades]
        percentages.sort()
        
        n = len(percentages)
        median = percentages[n//2] if n % 2 == 1 else (percentages[n//2-1] + percentages[n//2]) / 2
        
        grade_distribution = {}
        for grade in all_grades:
            letter = grade.letter_grade
            grade_distribution[letter] = grade_distribution.get(letter, 0) + 1
        
        return {
            "total_grades": len(all_grades),
            "average": sum(percentages) / len(percentages),
            "median": median,
            "min": min(percentages),
            "max": max(percentages),
            "grade_distribution": grade_distribution,
            "students_enrolled": len(set(g.student_id for g in all_grades))
        }
    
    def get_honor_roll(self, min_gpa: float = 3.5) -> List[Student]:
        """Get students on honor roll."""
        return [s for s in self.students.values() if s.gpa >= min_gpa and s.is_active]
    
    def get_students_at_risk(self, max_gpa: float = 2.0) -> List[Student]:
        """Get students at academic risk."""
        return [s for s in self.students.values() if s.gpa <= max_gpa and s.is_active]
    
    def generate_transcript(self, student_id: str) -> Dict[str, Any]:
        """Generate a complete transcript for a student."""
        student = self.get_student(student_id)
        if not student:
            return {"error": "Student not found"}
        
        grades = self.get_student_grades(student_id)
        subjects = list(set(g.subject for g in grades))
        
        transcript = {
            "student_info": asdict(student),
            "subjects": {},
            "overall_stats": {
                "total_grades": len(grades),
                "overall_average": 0.0,
                "credits_completed": student.credits_completed,
                "current_gpa": student.gpa
            }
        }
        
        total_percentage = 0.0
        subject_count = 0
        
        for subject in subjects:
            subject_grades = [g for g in grades if g.subject == subject]
            subject_avg = sum(g.percentage for g in subject_grades) / len(subject_grades)
            weighted_avg = self.calculate_weighted_grade(student_id, subject)
            
            transcript["subjects"][subject] = {
                "grades": [asdict(g) for g in subject_grades],
                "average": subject_avg,
                "weighted_average": weighted_avg,
                "letter_grade": self._percentage_to_letter(weighted_avg or subject_avg),
                "total_assignments": len(subject_grades)
            }
            
            total_percentage += weighted_avg or subject_avg
            subject_count += 1
        
        if subject_count > 0:
            transcript["overall_stats"]["overall_average"] = total_percentage / subject_count
        
        return transcript
    
    def export_data(self, filename: str, format: str = "json") -> bool:
        """Export all data to file."""
        try:
            if format.lower() == "json":
                data = {
                    "students": {k: asdict(v) for k, v in self.students.items()},
                    "grades": {k: [asdict(g) for g in v] for k, v in self.grades.items()},
                    "export_date": datetime.now().isoformat()
                }
                with open(filename, 'w') as f:
                    json.dump(data, f, indent=2, default=str)
            
            elif format.lower() == "csv":
                with open(filename, 'w', newline='') as f:
                    writer = csv.writer(f)
                    # Write header
                    writer.writerow([
                        'Student ID', 'First Name', 'Last Name', 'Email', 'Major',
                        'GPA', 'Subject', 'Grade Type', 'Points Earned', 'Points Possible',
                        'Percentage', 'Letter Grade', 'Date Assigned'
                    ])
                    
                    # Write data
                    for student_id, grades in self.grades.items():
                        student = self.students[student_id]
                        for grade in grades:
                            writer.writerow([
                                student.id, student.first_name, student.last_name,
                                student.email, student.major, student.gpa,
                                grade.subject, grade.grade_type.value,
                                grade.points_earned, grade.points_possible,
                                f"{grade.percentage:.2f}%", grade.letter_grade,
                                grade.date_assigned.strftime("%Y-%m-%d")
                            ])
            
            elif format.lower() == "pickle":
                data = {
                    "students": self.students,
                    "grades": self.grades
                }
                with open(filename, 'wb') as f:
                    pickle.dump(data, f)
            
            else:
                return False
            
            return True
        
        except Exception as e:
            print(f"Export error: {e}")
            return False
    
    def import_data(self, filename: str, format: str = "json") -> bool:
        """Import data from file."""
        try:
            if format.lower() == "json":
                with open(filename, 'r') as f:
                    data = json.load(f)
                
                # Import students
                for student_data in data.get("students", {}).values():
                    # Convert datetime strings back to datetime objects
                    student_data["date_of_birth"] = datetime.fromisoformat(student_data["date_of_birth"])
                    student_data["enrollment_date"] = datetime.fromisoformat(student_data["enrollment_date"])
                    student = Student(**student_data)
                    self.add_student(student)
                
                # Import grades
                for student_id, grades_data in data.get("grades", {}).items():
                    for grade_data in grades_data:
                        grade_data["grade_type"] = GradeType(grade_data["grade_type"])
                        grade_data["date_assigned"] = datetime.fromisoformat(grade_data["date_assigned"])
                        if grade_data["date_submitted"]:
                            grade_data["date_submitted"] = datetime.fromisoformat(grade_data["date_submitted"])
                        grade = Grade(**grade_data)
                        self.add_grade(grade)
            
            elif format.lower() == "pickle":
                with open(filename, 'rb') as f:
                    data = pickle.load(f)
                
                self.students = data["students"]
                self.grades = data["grades"]
            
            else:
                return False
            
            return True
        
        except Exception as e:
            print(f"Import error: {e}")
            return False
    
    def _percentage_to_letter(self, percentage: float) -> str:
        """Convert percentage to letter grade."""
        if percentage >= 97: return "A+"
        elif percentage >= 93: return "A"
        elif percentage >= 90: return "A-"
        elif percentage >= 87: return "B+"
        elif percentage >= 83: return "B"
        elif percentage >= 80: return "B-"
        elif percentage >= 77: return "C+"
        elif percentage >= 73: return "C"
        elif percentage >= 70: return "C-"
        elif percentage >= 67: return "D+"
        elif percentage >= 63: return "D"
        elif percentage >= 60: return "D-"
        else: return "F"
    
    def list_students(self, sort_by: str = "last_name") -> List[Student]:
        """List all students sorted by specified field."""
        students = list(self.students.values())
        
        if sort_by == "last_name":
            students.sort(key=lambda s: s.last_name)
        elif sort_by == "first_name":
            students.sort(key=lambda s: s.first_name)
        elif sort_by == "gpa":
            students.sort(key=lambda s: s.gpa, reverse=True)
        elif sort_by == "enrollment_date":
            students.sort(key=lambda s: s.enrollment_date)
        
        return students

def create_sample_data() -> StudentManager:
    """Create sample data for demonstration."""
    manager = StudentManager()
    
    # Sample students
    students_data = [
        ("STU001", "Alice", "Johnson", "alice.j@email.com", "555-0101", "2000-05-15", "Computer Science", 3.8),
        ("STU002", "Bob", "Smith", "bob.s@email.com", "555-0102", "1999-12-03", "Mathematics", 3.6),
        ("STU003", "Charlie", "Brown", "charlie.b@email.com", "555-0103", "2001-08-22", "Physics", 3.9),
        ("STU004", "Diana", "Prince", "diana.p@email.com", "555-0104", "2000-03-10", "Chemistry", 3.7),
        ("STU005", "Eva", "Martinez", "eva.m@email.com", "555-0105", "1999-11-18", "Biology", 3.5),
        ("STU006", "Frank", "Wilson", "frank.w@email.com", "555-0106", "2001-01-25", "Computer Science", 2.8),
        ("STU007", "Grace", "Lee", "grace.l@email.com", "555-0107", "2000-07-14", "Mathematics", 3.9),
        ("STU008", "Henry", "Taylor", "henry.t@email.com", "555-0108", "2001-04-30", "Physics", 2.1),
    ]
    
    for student_data in students_data:
        student = Student(
            id=student_data[0],
            first_name=student_data[1],
            last_name=student_data[2],
            email=student_data[3],
            phone=student_data[4],
            date_of_birth=datetime.strptime(student_data[5], "%Y-%m-%d"),
            enrollment_date=datetime(2020, 9, 1),
            major=student_data[6],
            gpa=student_data[7],
            credits_completed=60
        )
        manager.add_student(student)
    
    # Sample grades
    subjects = ["Calculus I", "Physics I", "Chemistry I", "Programming I", "English Composition"]
    grade_types = [GradeType.HOMEWORK, GradeType.QUIZ, GradeType.MIDTERM, GradeType.FINAL, GradeType.PROJECT]
    
    import random
    random.seed(42)  # For reproducible results
    
    for student_id in manager.students.keys():
        for subject in subjects[:3]:  # Each student takes 3 subjects
            for grade_type in grade_types:
                # Generate realistic grades based on student's GPA
                student_gpa = manager.students[student_id].gpa
                base_percentage = (student_gpa / 4.0) * 85 + 10  # Convert GPA to percentage base
                
                # Add some randomness
                percentage = base_percentage + random.uniform(-15, 15)
                percentage = max(0, min(100, percentage))  # Clamp to 0-100
                
                points_possible = random.choice([10, 20, 50, 100])
                points_earned = (percentage / 100) * points_possible
                
                grade = Grade(
                    id=str(uuid.uuid4()),
                    student_id=student_id,
                    subject=subject,
                    grade_type=grade_type,
                    points_earned=points_earned,
                    points_possible=points_possible,
                    date_assigned=datetime(2023, random.randint(1, 12), random.randint(1, 28)),
                    date_submitted=datetime(2023, random.randint(1, 12), random.randint(1, 28)),
                    comments="Good work!" if percentage > 80 else "Needs improvement"
                )
                
                manager.add_grade(grade)
    
    return manager

def main():
    print("Advanced Student Management System")
    print("=================================")
    
    # Create sample data
    manager = create_sample_data()
    
    print(f"\n1. System Overview:")
    print(f"Total students: {len(manager.students)}")
    print(f"Total grades recorded: {sum(len(grades) for grades in manager.grades.values())}")
    
    # List all students
    print(f"\n2. Student Roster (sorted by last name):")
    students = manager.list_students("last_name")
    for student in students:
        print(f"  {student.id}: {student.full_name} - {student.major} - GPA: {student.gpa:.2f}")
    
    # Search functionality
    print(f"\n3. Search Results for 'Computer':")
    search_results = manager.search_students("Computer")
    for student in search_results:
        print(f"  {student.full_name} - {student.major}")
    
    # Individual student analysis
    print(f"\n4. Individual Student Analysis (Alice Johnson):")
    alice = manager.get_student("STU001")
    if alice:
        print(f"  Name: {alice.full_name}")
        print(f"  Age: {alice.age}")
        print(f"  Major: {alice.major}")
        print(f"  GPA: {alice.gpa}")
        print(f"  Credits: {alice.credits_completed}")
        
        # Show grades for each subject
        alice_grades = manager.get_student_grades("STU001")
        subjects = list(set(g.subject for g in alice_grades))
        
        print(f"  Subjects and Averages:")
        for subject in subjects:
            avg = manager.calculate_grade_average("STU001", subject)
            weighted_avg = manager.calculate_weighted_grade("STU001", subject)
            letter = manager._percentage_to_letter(weighted_avg or avg)
            print(f"    {subject}: {avg:.1f}% (Weighted: {weighted_avg:.1f}%) - {letter}")
    
    # Class statistics
    print(f"\n5. Class Statistics for 'Calculus I':")
    stats = manager.get_class_statistics("Calculus I")
    if "error" not in stats:
        print(f"  Students enrolled: {stats['students_enrolled']}")
        print(f"  Total grades: {stats['total_grades']}")
        print(f"  Class average: {stats['average']:.1f}%")
        print(f"  Median: {stats['median']:.1f}%")
        print(f"  Range: {stats['min']:.1f}% - {stats['max']:.1f}%")
        print(f"  Grade distribution:")
        for letter, count in sorted(stats['grade_distribution'].items()):
            print(f"    {letter}: {count} students")
    
    # Honor roll and at-risk students
    print(f"\n6. Academic Standing:")
    honor_students = manager.get_honor_roll(3.5)
    at_risk_students = manager.get_students_at_risk(2.5)
    
    print(f"  Honor Roll (GPA ≥ 3.5): {len(honor_students)} students")
    for student in honor_students:
        print(f"    {student.full_name} - {student.gpa:.2f}")
    
    print(f"  At Risk (GPA ≤ 2.5): {len(at_risk_students)} students")
    for student in at_risk_students:
        print(f"    {student.full_name} - {student.gpa:.2f}")
    
    # Generate transcript
    print(f"\n7. Sample Transcript (Bob Smith):")
    transcript = manager.generate_transcript("STU002")
    if "error" not in transcript:
        student_info = transcript["student_info"]
        print(f"  Student: {student_info['first_name']} {student_info['last_name']}")
        print(f"  ID: {student_info['id']}")
        print(f"  Major: {student_info['major']}")
        print(f"  Overall GPA: {student_info['gpa']:.2f}")
        print(f"  Credits Completed: {student_info['credits_completed']}")
        
        print(f"  Course History:")
        for subject, subject_data in transcript["subjects"].items():
            print(f"    {subject}: {subject_data['weighted_average']:.1f}% ({subject_data['letter_grade']})")
            print(f"      Assignments completed: {subject_data['total_assignments']}")
    
    # Export demonstration
    print(f"\n8. Data Export:")
    print("  Exporting data to JSON format...")
    if manager.export_data("student_data_export.json", "json"):
        print("  ✓ JSON export successful")
    
    print("  Exporting data to CSV format...")
    if manager.export_data("student_data_export.csv", "csv"):
        print("  ✓ CSV export successful")
    
    # Performance metrics
    print(f"\n9. System Performance Metrics:")
    total_grades = sum(len(grades) for grades in manager.grades.values())
    active_students = sum(1 for s in manager.students.values() if s.is_active)
    
    print(f"  Active students: {active_students}/{len(manager.students)}")
    print(f"  Average grades per student: {total_grades/len(manager.students):.1f}")
    print(f"  Subjects being tracked: {len(set(g.subject for grades in manager.grades.values() for g in grades))}")
    
    # Advanced analytics
    print(f"\n10. Advanced Analytics:")
    all_grades = [g for grades in manager.grades.values() for g in grades]
    
    # Grade type analysis
    grade_type_stats = {}
    for grade_type in GradeType:
        type_grades = [g for g in all_grades if g.grade_type == grade_type]
        if type_grades:
            avg_percentage = sum(g.percentage for g in type_grades) / len(type_grades)
            grade_type_stats[grade_type.value] = {
                "count": len(type_grades),
                "average": avg_percentage
            }
    
    print(f"  Grade Type Performance:")
    for grade_type, stats in grade_type_stats.items():
        print(f"    {grade_type.title()}: {stats['average']:.1f}% avg ({stats['count']} grades)")
    
    # Late submission analysis
    late_submissions = [g for g in all_grades if g.is_late()]
    print(f"  Late submissions: {len(late_submissions)}/{len(all_grades)} ({len(late_submissions)/len(all_grades)*100:.1f}%)")

if __name__ == "__main__":
    main()