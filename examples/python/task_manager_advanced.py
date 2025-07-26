import json
import datetime
from typing import List, Dict, Optional
from dataclasses import dataclass, asdict
from pathlib import Path

@dataclass
class Task:
    id: int
    title: str
    description: str
    completed: bool = False
    created_at: str = None
    completed_at: Optional[str] = None
    priority: str = "medium"  # low, medium, high
    
    def __post_init__(self):
        if self.created_at is None:
            self.created_at = datetime.datetime.now().isoformat()
    
    def mark_completed(self):
        self.completed = True
        self.completed_at = datetime.datetime.now().isoformat()
    
    def mark_pending(self):
        self.completed = False
        self.completed_at = None

class TaskManager:
    def __init__(self, storage_file: str = "tasks.json"):
        self.storage_file = Path(storage_file)
        self.tasks: List[Task] = []
        self.next_id = 1
        self.load_tasks()
    
    def load_tasks(self):
        """Load tasks from JSON file"""
        if self.storage_file.exists():
            try:
                with open(self.storage_file, 'r', encoding='utf-8') as f:
                    data = json.load(f)
                    self.tasks = [Task(**task_data) for task_data in data.get('tasks', [])]
                    self.next_id = data.get('next_id', 1)
            except (json.JSONDecodeError, KeyError) as e:
                print(f"Error loading tasks: {e}")
                self.tasks = []
                self.next_id = 1
    
    def save_tasks(self):
        """Save tasks to JSON file"""
        data = {
            'tasks': [asdict(task) for task in self.tasks],
            'next_id': self.next_id
        }
        with open(self.storage_file, 'w', encoding='utf-8') as f:
            json.dump(data, f, indent=2, ensure_ascii=False)
    
    def add_task(self, title: str, description: str, priority: str = "medium") -> Task:
        """Add a new task"""
        task = Task(
            id=self.next_id,
            title=title,
            description=description,
            priority=priority
        )
        self.tasks.append(task)
        self.next_id += 1
        self.save_tasks()
        return task
    
    def get_task(self, task_id: int) -> Optional[Task]:
        """Get a task by ID"""
        return next((task for task in self.tasks if task.id == task_id), None)
    
    def complete_task(self, task_id: int) -> bool:
        """Mark a task as completed"""
        task = self.get_task(task_id)
        if task:
            task.mark_completed()
            self.save_tasks()
            return True
        return False
    
    def delete_task(self, task_id: int) -> bool:
        """Delete a task"""
        task = self.get_task(task_id)
        if task:
            self.tasks.remove(task)
            self.save_tasks()
            return True
        return False
    
    def get_tasks_by_status(self, completed: bool = False) -> List[Task]:
        """Get tasks by completion status"""
        return [task for task in self.tasks if task.completed == completed]
    
    def get_tasks_by_priority(self, priority: str) -> List[Task]:
        """Get tasks by priority"""
        return [task for task in self.tasks if task.priority == priority]
    
    def get_statistics(self) -> Dict[str, int]:
        """Get task statistics"""
        total = len(self.tasks)
        completed = len(self.get_tasks_by_status(True))
        pending = len(self.get_tasks_by_status(False))
        
        priority_counts = {}
        for priority in ["low", "medium", "high"]:
            priority_counts[priority] = len(self.get_tasks_by_priority(priority))
        
        return {
            "total": total,
            "completed": completed,
            "pending": pending,
            "completion_rate": round((completed / total * 100) if total > 0 else 0, 2),
            "priority_breakdown": priority_counts
        }
    
    def search_tasks(self, query: str) -> List[Task]:
        """Search tasks by title or description"""
        query = query.lower()
        return [
            task for task in self.tasks
            if query in task.title.lower() or query in task.description.lower()
        ]

def main():
    print("Advanced Task Management System")
    print("===============================")
    
    # Create task manager
    tm = TaskManager("demo_tasks.json")
    
    # Add sample tasks
    sample_tasks = [
        ("Implement user authentication", "Add login/logout functionality with JWT tokens", "high"),
        ("Write unit tests", "Create comprehensive test suite for all modules", "medium"),
        ("Update documentation", "Update README and API documentation", "low"),
        ("Database optimization", "Optimize slow queries and add indexes", "high"),
        ("UI/UX improvements", "Improve user interface design and usability", "medium"),
        ("Code refactoring", "Refactor legacy code for better maintainability", "medium"),
        ("Security audit", "Perform security review and fix vulnerabilities", "high"),
        ("Performance monitoring", "Set up application performance monitoring", "low"),
    ]
    
    # Clear existing tasks for demo
    tm.tasks = []
    tm.next_id = 1
    
    # Add sample tasks
    for title, description, priority in sample_tasks:
        task = tm.add_task(title, description, priority)
        print(f"Added task: {task.id} - {task.title} ({task.priority})")
    
    # Complete some tasks
    tm.complete_task(1)
    tm.complete_task(3)
    tm.complete_task(8)
    
    print(f"\nCompleted tasks: {len(tm.get_tasks_by_status(True))}")
    
    # Display statistics
    stats = tm.get_statistics()
    print(f"\nTask Statistics:")
    print(f"Total tasks: {stats['total']}")
    print(f"Completed: {stats['completed']}")
    print(f"Pending: {stats['pending']}")
    print(f"Completion rate: {stats['completion_rate']}%")
    print(f"Priority breakdown: {stats['priority_breakdown']}")
    
    # Search functionality
    print(f"\nSearching for 'test':")
    search_results = tm.search_tasks("test")
    for task in search_results:
        status = "✓" if task.completed else "○"
        print(f"  {status} {task.id}: {task.title}")
    
    # Display tasks by priority
    print(f"\nHigh priority tasks:")
    high_priority = tm.get_tasks_by_priority("high")
    for task in high_priority:
        status = "✓" if task.completed else "○"
        print(f"  {status} {task.id}: {task.title}")
    
    # Display pending tasks
    print(f"\nPending tasks:")
    pending_tasks = tm.get_tasks_by_status(False)
    for task in pending_tasks:
        print(f"  ○ {task.id}: {task.title} ({task.priority})")
    
    print(f"\nTask management demo completed!")
    print(f"Tasks saved to: {tm.storage_file}")

if __name__ == "__main__":
    main()