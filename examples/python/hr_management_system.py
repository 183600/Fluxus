import sqlite3
from datetime import datetime, timedelta
from typing import List, Optional, Dict, Any
from dataclasses import dataclass, asdict
from enum import Enum
import hashlib
import secrets
import json
import bcrypt

class UserRole(Enum):
    ADMIN = "admin"
    MANAGER = "manager"
    EMPLOYEE = "employee"
    INTERN = "intern"

class EmployeeStatus(Enum):
    ACTIVE = "active"
    INACTIVE = "inactive" 
    ON_LEAVE = "on_leave"
    TERMINATED = "terminated"

@dataclass
class Employee:
    id: Optional[int]
    employee_id: str
    first_name: str
    last_name: str
    email: str
    phone: str
    department: str
    position: str
    hire_date: datetime
    salary: float
    status: EmployeeStatus
    role: UserRole
    manager_id: Optional[int] = None
    created_at: Optional[datetime] = None
    updated_at: Optional[datetime] = None
    
    @property
    def full_name(self) -> str:
        return f"{self.first_name} {self.last_name}"
    
    @property
    def years_of_service(self) -> float:
        return (datetime.now() - self.hire_date).days / 365.25

@dataclass
class Attendance:
    id: Optional[int]
    employee_id: int
    date: datetime
    check_in: Optional[datetime]
    check_out: Optional[datetime]
    break_duration: int = 0  # minutes
    notes: str = ""
    
    @property
    def hours_worked(self) -> float:
        if self.check_in and self.check_out:
            total_minutes = (self.check_out - self.check_in).total_seconds() / 60
            return max(0, (total_minutes - self.break_duration) / 60)
        return 0.0

@dataclass
class LeaveRequest:
    id: Optional[int]
    employee_id: int
    leave_type: str
    start_date: datetime
    end_date: datetime
    days_requested: int
    reason: str
    status: str = "pending"  # pending, approved, rejected
    approved_by: Optional[int] = None
    created_at: Optional[datetime] = None

class HRDatabase:
    def __init__(self, db_path: str = "hr_system.db"):
        self.db_path = db_path
        self.init_database()
    
    def init_database(self):
        """Initialize database tables"""
        with sqlite3.connect(self.db_path) as conn:
            cursor = conn.cursor()
            
            # Employees table
            cursor.execute('''
                CREATE TABLE IF NOT EXISTS employees (
                    id INTEGER PRIMARY KEY AUTOINCREMENT,
                    employee_id TEXT UNIQUE NOT NULL,
                    first_name TEXT NOT NULL,
                    last_name TEXT NOT NULL,
                    email TEXT UNIQUE NOT NULL,
                    phone TEXT,
                    department TEXT NOT NULL,
                    position TEXT NOT NULL,
                    hire_date DATE NOT NULL,
                    salary REAL NOT NULL,
                    status TEXT NOT NULL,
                    role TEXT NOT NULL,
                    manager_id INTEGER,
                    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                    FOREIGN KEY (manager_id) REFERENCES employees (id)
                )
            ''')
            
            # Attendance table
            cursor.execute('''
                CREATE TABLE IF NOT EXISTS attendance (
                    id INTEGER PRIMARY KEY AUTOINCREMENT,
                    employee_id INTEGER NOT NULL,
                    date DATE NOT NULL,
                    check_in TIMESTAMP,
                    check_out TIMESTAMP,
                    break_duration INTEGER DEFAULT 0,
                    notes TEXT,
                    FOREIGN KEY (employee_id) REFERENCES employees (id),
                    UNIQUE(employee_id, date)
                )
            ''')
            
            # Leave requests table
            cursor.execute('''
                CREATE TABLE IF NOT EXISTS leave_requests (
                    id INTEGER PRIMARY KEY AUTOINCREMENT,
                    employee_id INTEGER NOT NULL,
                    leave_type TEXT NOT NULL,
                    start_date DATE NOT NULL,
                    end_date DATE NOT NULL,
                    days_requested INTEGER NOT NULL,
                    reason TEXT,
                    status TEXT DEFAULT 'pending',
                    approved_by INTEGER,
                    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                    FOREIGN KEY (employee_id) REFERENCES employees (id),
                    FOREIGN KEY (approved_by) REFERENCES employees (id)
                )
            ''')
            
            # User sessions table for authentication
            cursor.execute('''
                CREATE TABLE IF NOT EXISTS user_sessions (
                    id INTEGER PRIMARY KEY AUTOINCREMENT,
                    employee_id INTEGER NOT NULL,
                    session_token TEXT UNIQUE NOT NULL,
                    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                    expires_at TIMESTAMP NOT NULL,
                    FOREIGN KEY (employee_id) REFERENCES employees (id)
                )
            ''')
            
            conn.commit()

class HRManagementSystem:
    def __init__(self, db_path: str = "hr_system.db"):
        self.db = HRDatabase(db_path)
    
    def add_employee(self, employee: Employee) -> int:
        """Add a new employee to the system"""
        with sqlite3.connect(self.db.db_path) as conn:
            cursor = conn.cursor()
            
            cursor.execute('''
                INSERT INTO employees 
                (employee_id, first_name, last_name, email, phone, department, 
                 position, hire_date, salary, status, role, manager_id)
                VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
            ''', (
                employee.employee_id, employee.first_name, employee.last_name,
                employee.email, employee.phone, employee.department,
                employee.position, employee.hire_date.date(), employee.salary,
                employee.status.value, employee.role.value, employee.manager_id
            ))
            
            employee_id = cursor.lastrowid
            conn.commit()
            return employee_id
    
    def get_employee(self, employee_id: int) -> Optional[Employee]:
        """Get employee by ID"""
        with sqlite3.connect(self.db.db_path) as conn:
            cursor = conn.cursor()
            cursor.execute('SELECT * FROM employees WHERE id = ?', (employee_id,))
            row = cursor.fetchone()
            
            if row:
                return self._row_to_employee(row)
            return None
    
    def get_employees_by_department(self, department: str) -> List[Employee]:
        """Get all employees in a department"""
        with sqlite3.connect(self.db.db_path) as conn:
            cursor = conn.cursor()
            cursor.execute('SELECT * FROM employees WHERE department = ? AND status = ?', 
                         (department, EmployeeStatus.ACTIVE.value))
            rows = cursor.fetchall()
            
            return [self._row_to_employee(row) for row in rows]
    
    def update_employee_salary(self, employee_id: int, new_salary: float) -> bool:
        """Update employee salary"""
        with sqlite3.connect(self.db.db_path) as conn:
            cursor = conn.cursor()
            cursor.execute('''
                UPDATE employees 
                SET salary = ?, updated_at = CURRENT_TIMESTAMP 
                WHERE id = ?
            ''', (new_salary, employee_id))
            
            success = cursor.rowcount > 0
            conn.commit()
            return success
    
    def record_attendance(self, attendance: Attendance) -> int:
        """Record employee attendance"""
        with sqlite3.connect(self.db.db_path) as conn:
            cursor = conn.cursor()
            
            cursor.execute('''
                INSERT OR REPLACE INTO attendance 
                (employee_id, date, check_in, check_out, break_duration, notes)
                VALUES (?, ?, ?, ?, ?, ?)
            ''', (
                attendance.employee_id, attendance.date.date(),
                attendance.check_in, attendance.check_out,
                attendance.break_duration, attendance.notes
            ))
            
            attendance_id = cursor.lastrowid
            conn.commit()
            return attendance_id
    
    def get_employee_attendance(self, employee_id: int, start_date: datetime, 
                              end_date: datetime) -> List[Attendance]:
        """Get employee attendance for date range"""
        with sqlite3.connect(self.db.db_path) as conn:
            cursor = conn.cursor()
            cursor.execute('''
                SELECT * FROM attendance 
                WHERE employee_id = ? AND date BETWEEN ? AND ?
                ORDER BY date DESC
            ''', (employee_id, start_date.date(), end_date.date()))
            
            rows = cursor.fetchall()
            return [self._row_to_attendance(row) for row in rows]
    
    def submit_leave_request(self, leave_request: LeaveRequest) -> int:
        """Submit a leave request"""
        with sqlite3.connect(self.db.db_path) as conn:
            cursor = conn.cursor()
            
            cursor.execute('''
                INSERT INTO leave_requests 
                (employee_id, leave_type, start_date, end_date, days_requested, reason)
                VALUES (?, ?, ?, ?, ?, ?)
            ''', (
                leave_request.employee_id, leave_request.leave_type,
                leave_request.start_date.date(), leave_request.end_date.date(),
                leave_request.days_requested, leave_request.reason
            ))
            
            request_id = cursor.lastrowid
            conn.commit()
            return request_id
    
    def approve_leave_request(self, request_id: int, approved_by: int) -> bool:
        """Approve a leave request"""
        with sqlite3.connect(self.db.db_path) as conn:
            cursor = conn.cursor()
            cursor.execute('''
                UPDATE leave_requests 
                SET status = 'approved', approved_by = ?
                WHERE id = ? AND status = 'pending'
            ''', (approved_by, request_id))
            
            success = cursor.rowcount > 0
            conn.commit()
            return success
    
    def get_department_statistics(self, department: str) -> Dict[str, Any]:
        """Get department statistics"""
        with sqlite3.connect(self.db.db_path) as conn:
            cursor = conn.cursor()
            
            # Employee count by status
            cursor.execute('''
                SELECT status, COUNT(*) FROM employees 
                WHERE department = ? GROUP BY status
            ''', (department,))
            status_counts = dict(cursor.fetchall())
            
            # Average salary
            cursor.execute('''
                SELECT AVG(salary) FROM employees 
                WHERE department = ? AND status = 'active'
            ''', (department,))
            avg_salary = cursor.fetchone()[0] or 0
            
            # Recent hires (last 90 days)
            cursor.execute('''
                SELECT COUNT(*) FROM employees 
                WHERE department = ? AND hire_date >= date('now', '-90 days')
            ''', (department,))
            recent_hires = cursor.fetchone()[0]
            
            return {
                'department': department,
                'employee_counts': status_counts,
                'average_salary': round(avg_salary, 2),
                'recent_hires_90_days': recent_hires,
                'total_employees': sum(status_counts.values())
            }
    
    def get_attendance_report(self, department: str = None, 
                            start_date: datetime = None) -> Dict[str, Any]:
        """Generate attendance report"""
        if start_date is None:
            start_date = datetime.now() - timedelta(days=30)
        
        with sqlite3.connect(self.db.db_path) as conn:
            cursor = conn.cursor()
            
            query = '''
                SELECT e.department, e.full_name, 
                       COUNT(a.id) as days_worked,
                       AVG(
                           CASE WHEN a.check_in IS NOT NULL AND a.check_out IS NOT NULL
                           THEN (julianday(a.check_out) - julianday(a.check_in)) * 24 * 60 - a.break_duration
                           ELSE 0 END
                       ) / 60 as avg_hours_per_day
                FROM employees e
                LEFT JOIN attendance a ON e.id = a.employee_id 
                    AND a.date >= ?
                WHERE e.status = 'active'
            '''
            
            params = [start_date.date()]
            
            if department:
                query += ' AND e.department = ?'
                params.append(department)
            
            query += ' GROUP BY e.id, e.department, e.first_name, e.last_name'
            
            cursor.execute(query, params)
            results = cursor.fetchall()
            
            return {
                'report_period': f"From {start_date.date()}",
                'department_filter': department or "All Departments",
                'employee_attendance': [
                    {
                        'department': row[0],
                        'employee': row[1],
                        'days_worked': row[2],
                        'avg_hours_per_day': round(row[3] or 0, 2)
                    }
                    for row in results
                ]
            }
    
    def _row_to_employee(self, row) -> Employee:
        """Convert database row to Employee object"""
        return Employee(
            id=row[0], employee_id=row[1], first_name=row[2], last_name=row[3],
            email=row[4], phone=row[5], department=row[6], position=row[7],
            hire_date=datetime.fromisoformat(row[8]), salary=row[9],
            status=EmployeeStatus(row[10]), role=UserRole(row[11]),
            manager_id=row[12],
            created_at=datetime.fromisoformat(row[13]) if row[13] else None,
            updated_at=datetime.fromisoformat(row[14]) if row[14] else None
        )
    
    def _row_to_attendance(self, row) -> Attendance:
        """Convert database row to Attendance object"""
        return Attendance(
            id=row[0], employee_id=row[1],
            date=datetime.fromisoformat(row[2]),
            check_in=datetime.fromisoformat(row[3]) if row[3] else None,
            check_out=datetime.fromisoformat(row[4]) if row[4] else None,
            break_duration=row[5], notes=row[6] or ""
        )

def demo_hr_system():
    """Demonstrate HR management system functionality"""
    print("HR Management System Demo")
    print("=" * 50)
    
    # Initialize system
    hr_system = HRManagementSystem("demo_hr.db")
    
    # Add sample employees
    employees = [
        Employee(
            id=None, employee_id="EMP001", first_name="Alice", last_name="Johnson",
            email="alice.johnson@company.com", phone="555-0101",
            department="Engineering", position="Senior Developer",
            hire_date=datetime(2020, 1, 15), salary=95000.0,
            status=EmployeeStatus.ACTIVE, role=UserRole.EMPLOYEE
        ),
        Employee(
            id=None, employee_id="EMP002", first_name="Bob", last_name="Smith",
            email="bob.smith@company.com", phone="555-0102",
            department="Engineering", position="Team Lead",
            hire_date=datetime(2019, 3, 10), salary=110000.0,
            status=EmployeeStatus.ACTIVE, role=UserRole.MANAGER
        ),
        Employee(
            id=None, employee_id="EMP003", first_name="Carol", last_name="Davis",
            email="carol.davis@company.com", phone="555-0103",
            department="Marketing", position="Marketing Specialist",
            hire_date=datetime(2021, 6, 1), salary=65000.0,
            status=EmployeeStatus.ACTIVE, role=UserRole.EMPLOYEE
        ),
        Employee(
            id=None, employee_id="EMP004", first_name="David", last_name="Wilson",
            email="david.wilson@company.com", phone="555-0104",
            department="HR", position="HR Manager",
            hire_date=datetime(2018, 9, 12), salary=85000.0,
            status=EmployeeStatus.ACTIVE, role=UserRole.MANAGER
        )
    ]
    
    # Add employees to system
    employee_ids = []
    for emp in employees:
        try:
            emp_id = hr_system.add_employee(emp)
            employee_ids.append(emp_id)
            print(f"✓ Added employee: {emp.full_name} (ID: {emp_id})")
        except Exception as e:
            print(f"✗ Failed to add {emp.full_name}: {e}")
    
    print(f"\nAdded {len(employee_ids)} employees successfully")
    
    # Record some attendance
    print("\nRecording attendance data...")
    today = datetime.now()
    for i, emp_id in enumerate(employee_ids):
        for day_offset in range(5):  # Last 5 days
            date = today - timedelta(days=day_offset)
            attendance = Attendance(
                id=None, employee_id=emp_id, date=date,
                check_in=date.replace(hour=9, minute=0, second=0),
                check_out=date.replace(hour=17, minute=30, second=0),
                break_duration=60, notes="Regular workday"
            )
            hr_system.record_attendance(attendance)
    
    print("✓ Attendance records added")
    
    # Submit leave requests
    print("\nSubmitting leave requests...")
    leave_requests = [
        LeaveRequest(
            id=None, employee_id=employee_ids[0],
            leave_type="Vacation", 
            start_date=datetime.now() + timedelta(days=10),
            end_date=datetime.now() + timedelta(days=12),
            days_requested=3, reason="Family vacation"
        ),
        LeaveRequest(
            id=None, employee_id=employee_ids[2],
            leave_type="Sick Leave",
            start_date=datetime.now() + timedelta(days=5),
            end_date=datetime.now() + timedelta(days=5),
            days_requested=1, reason="Medical appointment"
        )
    ]
    
    for req in leave_requests:
        req_id = hr_system.submit_leave_request(req)
        print(f"✓ Leave request submitted (ID: {req_id})")
    
    # Generate reports
    print("\nGenerating Reports")
    print("-" * 30)
    
    # Department statistics
    for dept in ["Engineering", "Marketing", "HR"]:
        stats = hr_system.get_department_statistics(dept)
        print(f"\n{dept} Department:")
        print(f"  Total Employees: {stats['total_employees']}")
        print(f"  Average Salary: ${stats['average_salary']:,.2f}")
        print(f"  Recent Hires (90 days): {stats['recent_hires_90_days']}")
        print(f"  Status Distribution: {stats['employee_counts']}")
    
    # Attendance report
    print(f"\nAttendance Report (Last 30 days):")
    attendance_report = hr_system.get_attendance_report()
    for record in attendance_report['employee_attendance']:
        print(f"  {record['employee']} ({record['department']}): "
              f"{record['days_worked']} days, "
              f"{record['avg_hours_per_day']:.1f} avg hours/day")
    
    print("\nHR System demo completed successfully!")

if __name__ == "__main__":
    demo_hr_system()