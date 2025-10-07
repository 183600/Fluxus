#!/usr/bin/env python3
"""
测试Python的CSV、XML、YAML文件处理
"""

import csv
import xml.etree.ElementTree as ET
import json
import tempfile
import os

def test_csv_operations():
    """测试CSV文件操作"""
    print("=== Testing CSV Operations ===")
    
    # 创建测试数据
    data = [
        ['Name', 'Age', 'City', 'Salary'],
        ['Alice', '30', 'New York', '75000'],
        ['Bob', '25', 'San Francisco', '80000'],
        ['Charlie', '35', 'Chicago', '90000'],
        ['Diana', '28', 'Boston', '85000']
    ]
    
    # 写入CSV文件
    with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.csv', newline='') as f:
        csv_path = f.name
        writer = csv.writer(f)
        writer.writerows(data)
    
    try:
        print(f"CSV file created: {csv_path}")
        
        # 读取CSV文件
        print("\n--- Reading CSV file ---")
        with open(csv_path, 'r', newline='') as f:
            reader = csv.reader(f)
            for row_num, row in enumerate(reader):
                print(f"Row {row_num}: {row}")
        
        # 使用DictReader读取为字典
        print("\n--- Reading CSV with DictReader ---")
        with open(csv_path, 'r', newline='') as f:
            dict_reader = csv.DictReader(f)
            for row in dict_reader:
                print(f"Person: {row['Name']}, Age: {row['Age']}, City: {row['City']}, Salary: ${row['Salary']}")
        
        # 追加数据到CSV文件
        print("\n--- Appending to CSV file ---")
        with open(csv_path, 'a', newline='') as f:
            writer = csv.writer(f)
            writer.writerow(['Eve', '32', 'Seattle', '95000'])
        
        # 验证追加的数据
        print("After appending:")
        with open(csv_path, 'r', newline='') as f:
            reader = csv.reader(f)
            for i, row in enumerate(reader):
                if i >= len(data):  # 只显示新增的行
                    print(f"New row: {row}")
    
    finally:
        # 清理临时文件
        if os.path.exists(csv_path):
            os.unlink(csv_path)

def test_csv_advanced_features():
    """测试CSV高级特性"""
    print("\n=== Testing CSV Advanced Features ===")
    
    # 测试不同的分隔符和引号规则
    data = [
        ['Product', 'Description', 'Price'],
        ['Laptop', 'High-performance "gaming" laptop', '1299.99'],
        ['Mouse', 'Wireless, ergonomic design', '29.99'],
        ['Keyboard', 'Mechanical, RGB backlit', '89.99']
    ]
    
    with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.csv') as f:
        csv_path = f.name
        writer = csv.writer(f, delimiter=';', quotechar='"', quoting=csv.QUOTE_MINIMAL)
        writer.writerows(data)
    
    try:
        print(f"CSV with custom delimiter created: {csv_path}")
        
        # 读取自定义格式的CSV
        with open(csv_path, 'r') as f:
            content = f.read()
            print("CSV file content:")
            print(content)
        
        # 使用自定义设置读取
        with open(csv_path, 'r') as f:
            reader = csv.reader(f, delimiter=';', quotechar='"')
            for row in reader:
                print(f"Parsed row: {row}")
    
    finally:
        if os.path.exists(csv_path):
            os.unlink(csv_path)

def test_xml_operations():
    """测试XML文件操作"""
    print("\n=== Testing XML Operations ===")
    
    # 创建XML数据
    root = ET.Element("bookstore")
    
    # 添加书籍
    book1 = ET.SubElement(root, "book")
    book1.set("category", "fiction")
    
    title1 = ET.SubElement(book1, "title")
    title1.text = "The Great Gatsby"
    title1.set("lang", "en")
    
    author1 = ET.SubElement(book1, "author")
    author1.text = "F. Scott Fitzgerald"
    
    year1 = ET.SubElement(book1, "year")
    year1.text = "1925"
    
    price1 = ET.SubElement(book1, "price")
    price1.text = "12.99"
    
    # 第二本书
    book2 = ET.SubElement(root, "book")
    book2.set("category", "science")
    
    title2 = ET.SubElement(book2, "title")
    title2.text = "A Brief History of Time"
    title2.set("lang", "en")
    
    author2 = ET.SubElement(book2, "author")
    author2.text = "Stephen Hawking"
    
    year2 = ET.SubElement(book2, "year")
    year2.text = "1988"
    
    price2 = ET.SubElement(book2, "price")
    price2.text = "15.99"
    
    # 写入XML文件
    with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.xml') as f:
        xml_path = f.name
        tree = ET.ElementTree(root)
        tree.write(xml_path, encoding='unicode', xml_declaration=True)
    
    try:
        print(f"XML file created: {xml_path}")
        
        # 读取并解析XML
        print("\n--- Reading XML file ---")
        tree = ET.parse(xml_path)
        root = tree.getroot()
        
        print("Root element:", root.tag)
        print("Books in store:")
        
        for book in root.findall('book'):
            category = book.get('category')
            title = book.find('title').text
            author = book.find('author').text
            year = book.find('year').text
            price = book.find('price').text
            lang = book.find('title').get('lang')
            
            print(f"  Category: {category}")
            print(f"  Title: {title} ({lang})")
            print(f"  Author: {author}")
            print(f"  Year: {year}")
            print(f"  Price: ${price}")
            print()
        
        # 修改XML
        print("--- Modifying XML ---")
        new_book = ET.SubElement(root, "book")
        new_book.set("category", "technology")
        
        new_title = ET.SubElement(new_book, "title")
        new_title.text = "Clean Code"
        new_title.set("lang", "en")
        
        new_author = ET.SubElement(new_book, "author")
        new_author.text = "Robert C. Martin"
        
        new_year = ET.SubElement(new_book, "year")
        new_year.text = "2008"
        
        new_price = ET.SubElement(new_book, "price")
        new_price.text = "34.99"
        
        # 保存修改后的XML
        tree.write(xml_path, encoding='unicode', xml_declaration=True)
        
        print("XML file updated with new book")
        
        # 验证修改
        print("Updated XML content:")
        with open(xml_path, 'r') as f:
            print(f.read())
    
    finally:
        if os.path.exists(xml_path):
            os.unlink(xml_path)

def test_xml_search_and_modify():
    """测试XML搜索和修改"""
    print("\n=== Testing XML Search and Modify ===")
    
    xml_string = '''
    <library>
        <book id="1" genre="fiction">
            <title>Harry Potter</title>
            <author>J.K. Rowling</author>
            <year>1997</year>
            <price>15.99</price>
        </book>
        <book id="2" genre="fiction">
            <title>Lord of the Rings</title>
            <author>J.R.R. Tolkien</author>
            <year>1954</year>
            <price>12.99</price>
        </book>
        <book id="3" genre="science">
            <title>Cosmos</title>
            <author>Carl Sagan</author>
            <year>1980</year>
            <price>18.99</price>
        </book>
    </library>
    '''
    
    with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.xml') as f:
        xml_path = f.name
        f.write(xml_string.strip())
    
    try:
        # 解析XML
        tree = ET.parse(xml_path)
        root = tree.getroot()
        
        print("Original books:")
        for book in root.findall('book'):
            title = book.find('title').text
            price = book.find('price').text
            print(f"  {title}: ${price}")
        
        # 搜索特定书籍并修改价格
        for book in root.findall('book'):
            title = book.find('title').text
            if 'Harry Potter' in title:
                book.find('price').text = '12.99'  # 降价
                book.set('discount', 'true')  # 添加属性
                print(f"Updated price for {title}")
        
        # 搜索特定作者
        print("\nBooks by authors with 'J.' in name:")
        for book in root.findall('book'):
            author = book.find('author').text
            if 'J.' in author:
                title = book.find('title').text
                print(f"  {title} by {author}")
        
        # 添加新元素
        new_book = ET.SubElement(root, "book")
        new_book.set("id", "4")
        new_book.set("genre", "technology")
        
        ET.SubElement(new_book, "title").text = "The Pragmatic Programmer"
        ET.SubElement(new_book, "author").text = "Andrew Hunt, David Thomas"
        ET.SubElement(new_book, "year").text = "1999"
        ET.SubElement(new_book, "price").text = "42.99"
        
        # 删除元素
        for book in root.findall('book'):
            if book.find('title').text == 'Cosmos':
                root.remove(book)
                print("Removed Cosmos book")
        
        # 保存修改
        tree.write(xml_path, encoding='unicode', xml_declaration=True)
        
        print("\nFinal XML:")
        with open(xml_path, 'r') as f:
            print(f.read())
    
    finally:
        if os.path.exists(xml_path):
            os.unlink(xml_path)

def test_yaml_like_operations():
    """测试YAML风格的操作（使用JSON作为替代，因为YAML需要额外库）"""
    print("\n=== Testing YAML-like Operations (using JSON) ===")
    
    # 创建类似YAML的数据结构
    config = {
        'server': {
            'host': 'localhost',
            'port': 8080,
            'debug': True
        },
        'database': {
            'type': 'postgresql',
            'host': 'db.example.com',
            'port': 5432,
            'credentials': {
                'username': 'admin',
                'password': 'secret123'
            }
        },
        'features': {
            'logging': {
                'level': 'info',
                'file': '/var/log/app.log'
            },
            'caching': {
                'enabled': True,
                'ttl': 3600
            }
        }
    }
    
    with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.json') as f:
        json_path = f.name
        json.dump(config, f, indent=2)
    
    try:
        print(f"Configuration file created: {json_path}")
        
        # 读取配置
        with open(json_path, 'r') as f:
            loaded_config = json.load(f)
        
        print("Loaded configuration:")
        print(json.dumps(loaded_config, indent=2))
        
        # 修改配置
        loaded_config['server']['port'] = 9090
        loaded_config['features']['new_feature'] = {'enabled': True, 'priority': 'high'}
        
        # 保存修改
        with open(json_path, 'w') as f:
            json.dump(loaded_config, f, indent=2)
        
        print("\nUpdated configuration:")
        with open(json_path, 'r') as f:
            print(f.read())
    
    finally:
        if os.path.exists(json_path):
            os.unlink(json_path)

if __name__ == "__main__":
    test_csv_operations()
    test_csv_advanced_features()
    test_xml_operations()
    test_xml_search_and_modify()
    test_yaml_like_operations()
    print("\n=== All CSV/XML/JSON tests completed ===")