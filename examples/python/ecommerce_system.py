import datetime
import random

class Product:
    def __init__(self, product_id, name, price, quantity):
        self.product_id = product_id
        self.name = name
        self.price = price
        self.quantity = quantity
        self.created_at = datetime.datetime.now()

    def __str__(self):
        return f"Product({self.product_id}, {self.name}, ${self.price:.2f}, Qty: {self.quantity})"

class ShoppingCart:
    def __init__(self):
        self.items = {}
        self.created_at = datetime.datetime.now()

    def add_item(self, product, quantity):
        if product.product_id in self.items:
            self.items[product.product_id]['quantity'] += quantity
        else:
            self.items[product.product_id] = {
                'product': product,
                'quantity': quantity
            }
        print(f"Added {quantity} x {product.name} to cart")

    def remove_item(self, product_id):
        if product_id in self.items:
            removed_item = self.items.pop(product_id)
            print(f"Removed {removed_item['product'].name} from cart")
        else:
            print("Item not found in cart")

    def get_total(self):
        total = 0
        for item in self.items.values():
            total += item['product'].price * item['quantity']
        return total

    def show_cart(self):
        if not self.items:
            print("Cart is empty")
            return
        
        print("\n=== Shopping Cart ===")
        for item in self.items.values():
            product = item['product']
            quantity = item['quantity']
            subtotal = product.price * quantity
            print(f"{product.name} - ${product.price:.2f} x {quantity} = ${subtotal:.2f}")
        print(f"Total: ${self.get_total():.2f}")
        print("=====================")

class Order:
    def __init__(self, order_id, cart, customer_name):
        self.order_id = order_id
        self.cart = cart
        self.customer_name = customer_name
        self.total = cart.get_total()
        self.status = "pending"
        self.created_at = datetime.datetime.now()

    def process_order(self):
        self.status = "processing"
        print(f"Processing order {self.order_id} for {self.customer_name}")

    def complete_order(self):
        self.status = "completed"
        print(f"Order {self.order_id} completed!")

    def cancel_order(self):
        self.status = "cancelled"
        print(f"Order {self.order_id} cancelled")

    def get_order_summary(self):
        return f"Order {self.order_id}: {self.customer_name} - ${self.total:.2f} ({self.status})"

class ECommerceSystem:
    def __init__(self):
        self.products = {}
        self.orders = {}
        self.next_order_id = 1000

    def add_product(self, product):
        self.products[product.product_id] = product
        print(f"Product added: {product}")

    def get_product(self, product_id):
        return self.products.get(product_id)

    def list_products(self):
        print("\n=== Available Products ===")
        for product in self.products.values():
            print(product)
        print("==========================")

    def create_order(self, cart, customer_name):
        order_id = self.next_order_id
        self.next_order_id += 1
        order = Order(order_id, cart, customer_name)
        self.orders[order_id] = order
        return order

    def get_order(self, order_id):
        return self.orders.get(order_id)

    def list_orders(self):
        print("\n=== Orders ===")
        for order in self.orders.values():
            print(order.get_order_summary())
        print("===============")

def main():
    print("E-Commerce System Demo")
    
    # Initialize system
    ecommerce = ECommerceSystem()
    
    # Add products
    products = [
        Product("P001", "Laptop", 999.99, 10),
        Product("P002", "Mouse", 29.99, 50),
        Product("P003", "Keyboard", 79.99, 25),
        Product("P004", "Monitor", 299.99, 15),
        Product("P005", "Headphones", 149.99, 30)
    ]
    
    for product in products:
        ecommerce.add_product(product)
    
    ecommerce.list_products()
    
    # Create shopping cart and add items
    cart = ShoppingCart()
    cart.add_item(ecommerce.get_product("P001"), 1)  # Laptop
    cart.add_item(ecommerce.get_product("P002"), 2)  # Mouse x2
    cart.add_item(ecommerce.get_product("P003"), 1)  # Keyboard
    
    cart.show_cart()
    
    # Create order
    order = ecommerce.create_order(cart, "John Doe")
    print(f"\nOrder created: {order.get_order_summary()}")
    
    # Process order
    order.process_order()
    order.complete_order()
    
    # Create another order
    cart2 = ShoppingCart()
    cart2.add_item(ecommerce.get_product("P004"), 2)  # Monitor x2
    cart2.add_item(ecommerce.get_product("P005"), 1)  # Headphones
    
    order2 = ecommerce.create_order(cart2, "Jane Smith")
    order2.process_order()
    
    ecommerce.list_orders()

if __name__ == "__main__":
    main()