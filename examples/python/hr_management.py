import random
import statistics

class Employee:
    def __init__(self, emp_id, name, department, salary, hire_date):
        self.emp_id = emp_id
        self.name = name
        self.department = department
        self.salary = salary
        self.hire_date = hire_date
        self.performance_scores = []

    def add_performance_score(self, score):
        if 0 <= score <= 100:
            self.performance_scores.append(score)
            print(f"Performance score {score} added for {self.name}")
        else:
            print("Invalid performance score. Must be between 0 and 100.")

    def get_average_performance(self):
        if not self.performance_scores:
            return 0
        return statistics.mean(self.performance_scores)

    def give_raise(self, percentage):
        old_salary = self.salary
        self.salary += self.salary * (percentage / 100)
        print(f"{self.name}'s salary increased from ${old_salary:,.2f} to ${self.salary:,.2f}")

    def __str__(self):
        avg_performance = self.get_average_performance()
        return f"Employee({self.emp_id}, {self.name}, {self.department}, ${self.salary:,.2f}, Avg Performance: {avg_performance:.1f})"

class Department:
    def __init__(self, name, manager):
        self.name = name
        self.manager = manager
        self.employees = []
        self.budget = 0

    def add_employee(self, employee):
        self.employees.append(employee)
        print(f"{employee.name} added to {self.name} department")

    def remove_employee(self, emp_id):
        self.employees = [emp for emp in self.employees if emp.emp_id != emp_id]
        print(f"Employee {emp_id} removed from {self.name} department")

    def get_total_salary_cost(self):
        return sum(emp.salary for emp in self.employees)

    def get_average_salary(self):
        if not self.employees:
            return 0
        return self.get_total_salary_cost() / len(self.employees)

    def get_department_performance(self):
        if not self.employees:
            return 0
        performances = [emp.get_average_performance() for emp in self.employees if emp.performance_scores]
        return statistics.mean(performances) if performances else 0

    def list_employees(self):
        print(f"\n=== {self.name} Department ===")
        print(f"Manager: {self.manager}")
        print(f"Total Employees: {len(self.employees)}")
        print(f"Total Salary Cost: ${self.get_total_salary_cost():,.2f}")
        print(f"Average Salary: ${self.get_average_salary():,.2f}")
        print(f"Department Performance: {self.get_department_performance():.1f}")
        print("\nEmployees:")
        for emp in self.employees:
            print(f"  {emp}")
        print("=" * 40)

class HRSystem:
    def __init__(self, company_name):
        self.company_name = company_name
        self.employees = {}
        self.departments = {}
        self.next_emp_id = 1001

    def add_department(self, department):
        self.departments[department.name] = department
        print(f"Department '{department.name}' added to {self.company_name}")

    def hire_employee(self, name, department_name, salary, hire_date):
        if department_name not in self.departments:
            print(f"Department '{department_name}' does not exist")
            return None
        
        emp_id = self.next_emp_id
        self.next_emp_id += 1
        
        employee = Employee(emp_id, name, department_name, salary, hire_date)
        self.employees[emp_id] = employee
        self.departments[department_name].add_employee(employee)
        
        print(f"Employee {name} hired with ID {emp_id}")
        return employee

    def fire_employee(self, emp_id):
        if emp_id in self.employees:
            employee = self.employees[emp_id]
            department = self.departments[employee.department]
            department.remove_employee(emp_id)
            del self.employees[emp_id]
            print(f"Employee {employee.name} (ID: {emp_id}) has been terminated")
        else:
            print(f"Employee with ID {emp_id} not found")

    def get_employee(self, emp_id):
        return self.employees.get(emp_id)

    def promote_employee(self, emp_id, raise_percentage):
        employee = self.get_employee(emp_id)
        if employee:
            employee.give_raise(raise_percentage)
            print(f"{employee.name} has been promoted!")
        else:
            print(f"Employee with ID {emp_id} not found")

    def generate_company_report(self):
        print(f"\n=== {self.company_name} Company Report ===")
        print(f"Total Employees: {len(self.employees)}")
        print(f"Total Departments: {len(self.departments)}")
        
        total_payroll = sum(emp.salary for emp in self.employees.values())
        print(f"Total Payroll: ${total_payroll:,.2f}")
        
        if self.employees:
            avg_salary = total_payroll / len(self.employees)
            print(f"Average Salary: ${avg_salary:,.2f}")
        
        print("\nDepartment Summary:")
        for dept in self.departments.values():
            print(f"  {dept.name}: {len(dept.employees)} employees, "
                  f"${dept.get_total_salary_cost():,.2f} total cost")
        print("=" * 50)

    def conduct_performance_reviews(self):
        print("\nConducting performance reviews...")
        for employee in self.employees.values():
            # Simulate performance score
            score = random.randint(60, 100)
            employee.add_performance_score(score)

def main():
    print("HR Management System Demo")
    
    # Initialize HR system
    hr_system = HRSystem("TechCorp Inc.")
    
    # Create departments
    it_dept = Department("IT", "Alice Johnson")
    hr_dept = Department("HR", "Bob Smith")
    sales_dept = Department("Sales", "Carol Brown")
    
    hr_system.add_department(it_dept)
    hr_system.add_department(hr_dept)
    hr_system.add_department(sales_dept)
    
    # Hire employees
    employees_data = [
        ("John Doe", "IT", 85000, "2023-01-15"),
        ("Jane Smith", "IT", 92000, "2022-11-20"),
        ("Mike Johnson", "IT", 78000, "2023-03-10"),
        ("Sarah Wilson", "HR", 65000, "2022-08-05"),
        ("Tom Brown", "HR", 58000, "2023-05-12"),
        ("Lisa Davis", "Sales", 75000, "2022-12-01"),
        ("David Miller", "Sales", 82000, "2022-09-15"),
        ("Emily Taylor", "Sales", 68000, "2023-02-28"),
    ]
    
    hired_employees = []
    for name, dept, salary, hire_date in employees_data:
        emp = hr_system.hire_employee(name, dept, salary, hire_date)
        if emp:
            hired_employees.append(emp)
    
    # Conduct performance reviews
    hr_system.conduct_performance_reviews()
    
    # Add more performance scores
    for _ in range(2):
        for emp in hired_employees:
            score = random.randint(70, 95)
            emp.add_performance_score(score)
    
    # List all departments
    for dept in hr_system.departments.values():
        dept.list_employees()
    
    # Promote some employees
    if len(hired_employees) >= 3:
        hr_system.promote_employee(hired_employees[0].emp_id, 10)
        hr_system.promote_employee(hired_employees[2].emp_id, 8)
    
    # Generate company report
    hr_system.generate_company_report()
    
    # Fire an employee
    if len(hired_employees) >= 1:
        hr_system.fire_employee(hired_employees[-1].emp_id)
    
    # Final report
    hr_system.generate_company_report()

if __name__ == "__main__":
    main()