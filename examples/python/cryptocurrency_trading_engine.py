import asyncio
import json
import time
import hashlib
import hmac
import base64
import uuid
from datetime import datetime, timedelta
from typing import Dict, List, Optional, Any, Union
from dataclasses import dataclass, asdict, field
from decimal import Decimal
import sqlite3
import threading
import logging
from enum import Enum
from collections import defaultdict
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from concurrent.futures import ThreadPoolExecutor
import warnings

warnings.filterwarnings('ignore')

class OrderType(Enum):
    MARKET = "market"
    LIMIT = "limit"
    STOP_LOSS = "stop_loss"
    TAKE_PROFIT = "take_profit"

class OrderSide(Enum):
    BUY = "buy"
    SELL = "sell"

class OrderStatus(Enum):
    PENDING = "pending"
    FILLED = "filled"
    CANCELLED = "cancelled"
    REJECTED = "rejected"

@dataclass
class Asset:
    symbol: str
    name: str
    price: Decimal
    volume: Decimal = Decimal('0')
    market_cap: Decimal = Decimal('0')
    last_updated: datetime = field(default_factory=datetime.now)

@dataclass
class Order:
    order_id: str
    user_id: str
    symbol: str
    order_type: OrderType
    side: OrderSide
    quantity: Decimal
    price: Optional[Decimal]
    status: OrderStatus = OrderStatus.PENDING
    created_at: datetime = field(default_factory=datetime.now)
    filled_at: Optional[datetime] = None
    filled_quantity: Decimal = Decimal('0')
    filled_price: Optional[Decimal] = None

@dataclass
class Trade:
    trade_id: str
    buyer_order_id: str
    seller_order_id: str
    symbol: str
    quantity: Decimal
    price: Decimal
    timestamp: datetime = field(default_factory=datetime.now)

@dataclass
class Portfolio:
    user_id: str
    balances: Dict[str, Decimal] = field(default_factory=dict)
    total_value: Decimal = Decimal('0')
    profit_loss: Decimal = Decimal('0')
    last_updated: datetime = field(default_factory=datetime.now)

class TradingEngine:
    def __init__(self, db_path: str = "trading_engine.db"):
        self.db_path = db_path
        self.assets: Dict[str, Asset] = {}
        self.orders: Dict[str, Order] = {}
        self.trades: List[Trade] = []
        self.portfolios: Dict[str, Portfolio] = {}
        self.order_books: Dict[str, Dict[str, List[Order]]] = defaultdict(lambda: {"buy": [], "sell": []})
        
        # Thread safety
        self.lock = threading.RLock()
        
        # Market data simulation
        self.market_data_thread = None
        self.running = False
        
        # Initialize database and logging
        self._init_database()
        self._setup_logging()
        
        # Load sample assets
        self._load_sample_assets()
    
    def _init_database(self):
        """Initialize SQLite database for persistent storage"""
        conn = sqlite3.connect(self.db_path)
        cursor = conn.cursor()
        
        # Assets table
        cursor.execute('''
            CREATE TABLE IF NOT EXISTS assets (
                symbol TEXT PRIMARY KEY,
                name TEXT,
                price REAL,
                volume REAL,
                market_cap REAL,
                last_updated TEXT
            )
        ''')
        
        # Orders table
        cursor.execute('''
            CREATE TABLE IF NOT EXISTS orders (
                order_id TEXT PRIMARY KEY,
                user_id TEXT,
                symbol TEXT,
                order_type TEXT,
                side TEXT,
                quantity REAL,
                price REAL,
                status TEXT,
                created_at TEXT,
                filled_at TEXT,
                filled_quantity REAL,
                filled_price REAL
            )
        ''')
        
        # Trades table
        cursor.execute('''
            CREATE TABLE IF NOT EXISTS trades (
                trade_id TEXT PRIMARY KEY,
                buyer_order_id TEXT,
                seller_order_id TEXT,
                symbol TEXT,
                quantity REAL,
                price REAL,
                timestamp TEXT
            )
        ''')
        
        # Portfolios table
        cursor.execute('''
            CREATE TABLE IF NOT EXISTS portfolios (
                user_id TEXT PRIMARY KEY,
                balances TEXT,
                total_value REAL,
                profit_loss REAL,
                last_updated TEXT
            )
        ''')
        
        # Market data table
        cursor.execute('''
            CREATE TABLE IF NOT EXISTS market_data (
                symbol TEXT,
                price REAL,
                volume REAL,
                timestamp TEXT,
                PRIMARY KEY (symbol, timestamp)
            )
        ''')
        
        conn.commit()
        conn.close()
    
    def _setup_logging(self):
        """Setup logging configuration"""
        logging.basicConfig(
            level=logging.INFO,
            format='%(asctime)s - %(name)s - %(levelname)s - %(message)s'
        )
        self.logger = logging.getLogger(__name__)
    
    def _load_sample_assets(self):
        """Load sample cryptocurrency assets"""
        sample_assets = [
            Asset("BTC", "Bitcoin", Decimal('45000.00')),
            Asset("ETH", "Ethereum", Decimal('3200.00')),
            Asset("ADA", "Cardano", Decimal('1.25')),
            Asset("DOT", "Polkadot", Decimal('25.50')),
            Asset("LINK", "Chainlink", Decimal('28.75')),
            Asset("UNI", "Uniswap", Decimal('22.30')),
            Asset("AVAX", "Avalanche", Decimal('85.40')),
            Asset("SOL", "Solana", Decimal('95.60')),
            Asset("MATIC", "Polygon", Decimal('1.85')),
            Asset("ATOM", "Cosmos", Decimal('32.10'))
        ]
        
        for asset in sample_assets:
            self.assets[asset.symbol] = asset
            self._save_asset(asset)
    
    def _save_asset(self, asset: Asset):
        """Save asset to database"""
        conn = sqlite3.connect(self.db_path)
        cursor = conn.cursor()
        
        cursor.execute('''
            INSERT OR REPLACE INTO assets 
            (symbol, name, price, volume, market_cap, last_updated)
            VALUES (?, ?, ?, ?, ?, ?)
        ''', (
            asset.symbol, asset.name, float(asset.price), 
            float(asset.volume), float(asset.market_cap),
            asset.last_updated.isoformat()
        ))
        
        conn.commit()
        conn.close()
    
    def _save_order(self, order: Order):
        """Save order to database"""
        conn = sqlite3.connect(self.db_path)
        cursor = conn.cursor()
        
        cursor.execute('''
            INSERT OR REPLACE INTO orders 
            (order_id, user_id, symbol, order_type, side, quantity, price, 
             status, created_at, filled_at, filled_quantity, filled_price)
            VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
        ''', (
            order.order_id, order.user_id, order.symbol,
            order.order_type.value, order.side.value,
            float(order.quantity), float(order.price) if order.price else None,
            order.status.value, order.created_at.isoformat(),
            order.filled_at.isoformat() if order.filled_at else None,
            float(order.filled_quantity),
            float(order.filled_price) if order.filled_price else None
        ))
        
        conn.commit()
        conn.close()
    
    def _save_trade(self, trade: Trade):
        """Save trade to database"""
        conn = sqlite3.connect(self.db_path)
        cursor = conn.cursor()
        
        cursor.execute('''
            INSERT INTO trades 
            (trade_id, buyer_order_id, seller_order_id, symbol, quantity, price, timestamp)
            VALUES (?, ?, ?, ?, ?, ?, ?)
        ''', (
            trade.trade_id, trade.buyer_order_id, trade.seller_order_id,
            trade.symbol, float(trade.quantity), float(trade.price),
            trade.timestamp.isoformat()
        ))
        
        conn.commit()
        conn.close()
    
    def create_user_portfolio(self, user_id: str, initial_balance: Dict[str, Decimal] = None) -> Portfolio:
        """Create a new user portfolio"""
        if initial_balance is None:
            initial_balance = {"USD": Decimal('10000.00')}  # $10,000 starting balance
        
        portfolio = Portfolio(
            user_id=user_id,
            balances=initial_balance.copy()
        )
        
        with self.lock:
            self.portfolios[user_id] = portfolio
        
        self.logger.info(f"Created portfolio for user {user_id}")
        return portfolio
    
    def place_order(self, user_id: str, symbol: str, order_type: OrderType, 
                   side: OrderSide, quantity: Decimal, price: Optional[Decimal] = None) -> str:
        """Place a new order"""
        
        # Validate inputs
        if symbol not in self.assets:
            raise ValueError(f"Unknown symbol: {symbol}")
        
        if quantity <= 0:
            raise ValueError("Quantity must be positive")
        
        if order_type == OrderType.LIMIT and price is None:
            raise ValueError("Limit orders require a price")
        
        # Create order
        order_id = str(uuid.uuid4())
        
        if order_type == OrderType.MARKET:
            price = self.assets[symbol].price
        
        order = Order(
            order_id=order_id,
            user_id=user_id,
            symbol=symbol,
            order_type=order_type,
            side=side,
            quantity=quantity,
            price=price
        )
        
        # Validate portfolio balance
        if not self._validate_order_balance(order):
            order.status = OrderStatus.REJECTED
            self.logger.warning(f"Order {order_id} rejected due to insufficient balance")
        else:
            with self.lock:
                self.orders[order_id] = order
                self.order_books[symbol][side.value].append(order)
                self._save_order(order)
            
            self.logger.info(f"Order placed: {order_id} - {side.value} {quantity} {symbol}")
            
            # Try to match the order
            asyncio.create_task(self._match_orders(symbol))
        
        return order_id
    
    def _validate_order_balance(self, order: Order) -> bool:
        """Validate if user has sufficient balance for the order"""
        if order.user_id not in self.portfolios:
            return False
        
        portfolio = self.portfolios[order.user_id]
        
        if order.side == OrderSide.BUY:
            # Check USD balance for buy orders
            required_amount = order.quantity * order.price
            usd_balance = portfolio.balances.get("USD", Decimal('0'))
            return usd_balance >= required_amount
        else:
            # Check asset balance for sell orders
            asset_balance = portfolio.balances.get(order.symbol, Decimal('0'))
            return asset_balance >= order.quantity
    
    async def _match_orders(self, symbol: str):
        """Match buy and sell orders for a symbol"""
        with self.lock:
            buy_orders = sorted(
                [o for o in self.order_books[symbol]["buy"] if o.status == OrderStatus.PENDING],
                key=lambda x: (-x.price, x.created_at)  # Highest price first, then FIFO
            )
            
            sell_orders = sorted(
                [o for o in self.order_books[symbol]["sell"] if o.status == OrderStatus.PENDING],
                key=lambda x: (x.price, x.created_at)  # Lowest price first, then FIFO
            )
            
            trades_executed = []
            
            while buy_orders and sell_orders:
                buy_order = buy_orders[0]
                sell_order = sell_orders[0]
                
                # Check if orders can be matched
                if buy_order.price < sell_order.price:
                    break
                
                # Calculate trade details
                trade_quantity = min(
                    buy_order.quantity - buy_order.filled_quantity,
                    sell_order.quantity - sell_order.filled_quantity
                )
                
                trade_price = sell_order.price  # Price taker gets maker's price
                
                # Create trade
                trade = Trade(
                    trade_id=str(uuid.uuid4()),
                    buyer_order_id=buy_order.order_id,
                    seller_order_id=sell_order.order_id,
                    symbol=symbol,
                    quantity=trade_quantity,
                    price=trade_price
                )
                
                # Update orders
                buy_order.filled_quantity += trade_quantity
                sell_order.filled_quantity += trade_quantity
                
                if buy_order.filled_quantity >= buy_order.quantity:
                    buy_order.status = OrderStatus.FILLED
                    buy_order.filled_at = datetime.now()
                    buy_order.filled_price = trade_price
                    buy_orders.pop(0)
                
                if sell_order.filled_quantity >= sell_order.quantity:
                    sell_order.status = OrderStatus.FILLED
                    sell_order.filled_at = datetime.now()
                    sell_order.filled_price = trade_price
                    sell_orders.pop(0)
                
                # Update portfolios
                self._execute_trade(trade, buy_order, sell_order)
                
                # Save trade and orders
                self.trades.append(trade)
                self._save_trade(trade)
                self._save_order(buy_order)
                self._save_order(sell_order)
                
                trades_executed.append(trade)
                
                # Update asset price
                self.assets[symbol].price = trade_price
                self._save_asset(self.assets[symbol])
                
                self.logger.info(f"Trade executed: {trade.trade_id} - {trade_quantity} {symbol} @ {trade_price}")
            
            return trades_executed
    
    def _execute_trade(self, trade: Trade, buy_order: Order, sell_order: Order):
        """Execute a trade by updating user portfolios"""
        buyer_portfolio = self.portfolios[buy_order.user_id]
        seller_portfolio = self.portfolios[sell_order.user_id]
        
        trade_value = trade.quantity * trade.price
        
        # Update buyer's portfolio
        buyer_portfolio.balances["USD"] -= trade_value
        buyer_portfolio.balances[trade.symbol] = buyer_portfolio.balances.get(trade.symbol, Decimal('0')) + trade.quantity
        
        # Update seller's portfolio
        seller_portfolio.balances["USD"] += trade_value
        seller_portfolio.balances[trade.symbol] -= trade.quantity
        
        # Update timestamps
        buyer_portfolio.last_updated = datetime.now()
        seller_portfolio.last_updated = datetime.now()
    
    def cancel_order(self, order_id: str) -> bool:
        """Cancel an order"""
        if order_id not in self.orders:
            return False
        
        order = self.orders[order_id]
        
        if order.status != OrderStatus.PENDING:
            return False
        
        with self.lock:
            order.status = OrderStatus.CANCELLED
            
            # Remove from order book
            symbol_book = self.order_books[order.symbol][order.side.value]
            if order in symbol_book:
                symbol_book.remove(order)
            
            self._save_order(order)
        
        self.logger.info(f"Order cancelled: {order_id}")
        return True
    
    def get_order_book(self, symbol: str) -> Dict[str, List[Dict]]:
        """Get current order book for a symbol"""
        if symbol not in self.assets:
            return {"buy": [], "sell": []}
        
        with self.lock:
            buy_orders = [
                {
                    "price": float(order.price),
                    "quantity": float(order.quantity - order.filled_quantity),
                    "order_id": order.order_id
                }
                for order in sorted(self.order_books[symbol]["buy"], 
                                  key=lambda x: -x.price)
                if order.status == OrderStatus.PENDING
            ]
            
            sell_orders = [
                {
                    "price": float(order.price),
                    "quantity": float(order.quantity - order.filled_quantity),
                    "order_id": order.order_id
                }
                for order in sorted(self.order_books[symbol]["sell"], 
                                  key=lambda x: x.price)
                if order.status == OrderStatus.PENDING
            ]
        
        return {"buy": buy_orders, "sell": sell_orders}
    
    def get_portfolio(self, user_id: str) -> Optional[Portfolio]:
        """Get user's portfolio"""
        portfolio = self.portfolios.get(user_id)
        if portfolio:
            # Update total value
            total_value = Decimal('0')
            for symbol, balance in portfolio.balances.items():
                if symbol == "USD":
                    total_value += balance
                elif symbol in self.assets:
                    total_value += balance * self.assets[symbol].price
            
            portfolio.total_value = total_value
        
        return portfolio
    
    def get_market_data(self, symbol: str, limit: int = 100) -> List[Dict]:
        """Get historical market data for a symbol"""
        conn = sqlite3.connect(self.db_path)
        cursor = conn.cursor()
        
        cursor.execute('''
            SELECT price, volume, timestamp 
            FROM market_data 
            WHERE symbol = ? 
            ORDER BY timestamp DESC 
            LIMIT ?
        ''', (symbol, limit))
        
        data = []
        for price, volume, timestamp in cursor.fetchall():
            data.append({
                "price": price,
                "volume": volume,
                "timestamp": timestamp
            })
        
        conn.close()
        return data
    
    def start_market_simulation(self):
        """Start market data simulation"""
        self.running = True
        self.market_data_thread = threading.Thread(target=self._simulate_market_data, daemon=True)
        self.market_data_thread.start()
        self.logger.info("Market simulation started")
    
    def stop_market_simulation(self):
        """Stop market data simulation"""
        self.running = False
        if self.market_data_thread:
            self.market_data_thread.join()
        self.logger.info("Market simulation stopped")
    
    def _simulate_market_data(self):
        """Simulate realistic market data"""
        while self.running:
            try:
                with self.lock:
                    for symbol, asset in self.assets.items():
                        # Simple random walk for price simulation
                        change_percent = np.random.normal(0, 0.02)  # 2% volatility
                        new_price = asset.price * (1 + Decimal(str(change_percent)))
                        
                        # Ensure price doesn't go below a minimum
                        min_price = Decimal('0.01')
                        asset.price = max(new_price, min_price)
                        
                        # Simulate volume
                        asset.volume = Decimal(str(np.random.exponential(1000)))
                        asset.last_updated = datetime.now()
                        
                        # Save to database
                        self._save_market_data(symbol, asset.price, asset.volume)
                
                time.sleep(5)  # Update every 5 seconds
                
            except Exception as e:
                self.logger.error(f"Error in market simulation: {e}")
                time.sleep(1)
    
    def _save_market_data(self, symbol: str, price: Decimal, volume: Decimal):
        """Save market data point to database"""
        conn = sqlite3.connect(self.db_path)
        cursor = conn.cursor()
        
        cursor.execute('''
            INSERT INTO market_data (symbol, price, volume, timestamp)
            VALUES (?, ?, ?, ?)
        ''', (symbol, float(price), float(volume), datetime.now().isoformat()))
        
        conn.commit()
        conn.close()
    
    def generate_trading_report(self, user_id: str = None) -> Dict[str, Any]:
        """Generate comprehensive trading report"""
        conn = sqlite3.connect(self.db_path)
        
        report = {
            "generated_at": datetime.now().isoformat(),
            "total_trades": len(self.trades),
            "active_orders": len([o for o in self.orders.values() if o.status == OrderStatus.PENDING])
        }
        
        if user_id:
            # User-specific report
            user_orders = [o for o in self.orders.values() if o.user_id == user_id]
            user_trades = []
            
            for trade in self.trades:
                buy_order = self.orders.get(trade.buyer_order_id)
                sell_order = self.orders.get(trade.seller_order_id)
                
                if (buy_order and buy_order.user_id == user_id) or \
                   (sell_order and sell_order.user_id == user_id):
                    user_trades.append(trade)
            
            portfolio = self.get_portfolio(user_id)
            
            report.update({
                "user_id": user_id,
                "total_orders": len(user_orders),
                "filled_orders": len([o for o in user_orders if o.status == OrderStatus.FILLED]),
                "cancelled_orders": len([o for o in user_orders if o.status == OrderStatus.CANCELLED]),
                "user_trades": len(user_trades),
                "portfolio": asdict(portfolio) if portfolio else None
            })
        
        else:
            # System-wide report
            df_trades = pd.DataFrame([
                {
                    "symbol": t.symbol,
                    "quantity": float(t.quantity),
                    "price": float(t.price),
                    "value": float(t.quantity * t.price),
                    "timestamp": t.timestamp
                }
                for t in self.trades
            ])
            
            if not df_trades.empty:
                report.update({
                    "total_volume": float(df_trades["value"].sum()),
                    "average_trade_size": float(df_trades["value"].mean()),
                    "most_traded_symbol": df_trades["symbol"].mode().iloc[0] if not df_trades["symbol"].mode().empty else None,
                    "price_ranges": {}
                })
                
                # Price ranges by symbol
                for symbol in df_trades["symbol"].unique():
                    symbol_data = df_trades[df_trades["symbol"] == symbol]
                    report["price_ranges"][symbol] = {
                        "min": float(symbol_data["price"].min()),
                        "max": float(symbol_data["price"].max()),
                        "current": float(self.assets[symbol].price)
                    }
        
        conn.close()
        return report
    
    def plot_market_analysis(self, symbols: List[str] = None, save_plots: bool = True):
        """Generate market analysis plots"""
        if symbols is None:
            symbols = list(self.assets.keys())[:5]  # Top 5 symbols
        
        fig, axes = plt.subplots(2, 2, figsize=(15, 10))
        fig.suptitle('Trading Engine Market Analysis', fontsize=16)
        
        # Plot 1: Current prices
        prices = [float(self.assets[symbol].price) for symbol in symbols]
        axes[0, 0].bar(symbols, prices, color='skyblue', alpha=0.7)
        axes[0, 0].set_title('Current Asset Prices')
        axes[0, 0].set_ylabel('Price (USD)')
        axes[0, 0].tick_params(axis='x', rotation=45)
        
        # Plot 2: Trading volume (if trades exist)
        if self.trades:
            df_trades = pd.DataFrame([
                {"symbol": t.symbol, "volume": float(t.quantity * t.price)}
                for t in self.trades
            ])
            
            volume_by_symbol = df_trades.groupby("symbol")["volume"].sum()
            
            axes[0, 1].pie(volume_by_symbol.values, labels=volume_by_symbol.index,
                          autopct='%1.1f%%', startangle=90)
            axes[0, 1].set_title('Trading Volume Distribution')
        else:
            axes[0, 1].text(0.5, 0.5, 'No trades yet', ha='center', va='center')
            axes[0, 1].set_title('Trading Volume Distribution')
        
        # Plot 3: Order book depth (example for first symbol)
        if symbols:
            symbol = symbols[0]
            order_book = self.get_order_book(symbol)
            
            buy_prices = [order["price"] for order in order_book["buy"]]
            buy_quantities = [order["quantity"] for order in order_book["buy"]]
            sell_prices = [order["price"] for order in order_book["sell"]]
            sell_quantities = [order["quantity"] for order in order_book["sell"]]
            
            axes[1, 0].bar(buy_prices, buy_quantities, color='green', alpha=0.7, label='Buy Orders')
            axes[1, 0].bar(sell_prices, sell_quantities, color='red', alpha=0.7, label='Sell Orders')
            axes[1, 0].set_title(f'Order Book Depth - {symbol}')
            axes[1, 0].set_xlabel('Price')
            axes[1, 0].set_ylabel('Quantity')
            axes[1, 0].legend()
        
        # Plot 4: Portfolio distribution (if portfolios exist)
        if self.portfolios:
            total_values = []
            user_ids = []
            
            for user_id, portfolio in self.portfolios.items():
                portfolio_updated = self.get_portfolio(user_id)
                total_values.append(float(portfolio_updated.total_value))
                user_ids.append(user_id[:8] + "...")  # Truncate for display
            
            axes[1, 1].bar(user_ids, total_values, color='orange', alpha=0.7)
            axes[1, 1].set_title('Portfolio Values by User')
            axes[1, 1].set_ylabel('Total Value (USD)')
            axes[1, 1].tick_params(axis='x', rotation=45)
        else:
            axes[1, 1].text(0.5, 0.5, 'No portfolios yet', ha='center', va='center')
            axes[1, 1].set_title('Portfolio Values by User')
        
        plt.tight_layout()
        
        if save_plots:
            plt.savefig('trading_analysis.png', dpi=300, bbox_inches='tight')
            self.logger.info("Market analysis plot saved as 'trading_analysis.png'")
        
        plt.show()

async def demo_trading_engine():
    """Demonstrate the trading engine with sample trading scenario"""
    print("Advanced Trading Engine Demo")
    print("=" * 50)
    
    # Initialize trading engine
    engine = TradingEngine()
    
    # Start market simulation
    engine.start_market_simulation()
    
    # Create demo users
    users = ["alice", "bob", "charlie"]
    for user in users:
        engine.create_user_portfolio(user)
        print(f"Created portfolio for {user}")
    
    # Wait for some market data
    await asyncio.sleep(2)
    
    # Place some sample orders
    print("\nPlacing sample orders...")
    
    # Alice places buy orders
    alice_btc_order = engine.place_order("alice", "BTC", OrderType.LIMIT, 
                                        OrderSide.BUY, Decimal('0.1'), Decimal('44000'))
    alice_eth_order = engine.place_order("alice", "ETH", OrderType.LIMIT, 
                                        OrderSide.BUY, Decimal('1.0'), Decimal('3100'))
    
    # Bob places sell orders
    bob_portfolio = engine.get_portfolio("bob")
    bob_portfolio.balances["BTC"] = Decimal('0.5')  # Give Bob some BTC to sell
    bob_portfolio.balances["ETH"] = Decimal('2.0')  # Give Bob some ETH to sell
    
    bob_btc_order = engine.place_order("bob", "BTC", OrderType.LIMIT, 
                                      OrderSide.SELL, Decimal('0.1'), Decimal('44500'))
    bob_eth_order = engine.place_order("bob", "ETH", OrderType.LIMIT, 
                                      OrderSide.SELL, Decimal('1.0'), Decimal('3150'))
    
    # Charlie places market orders
    charlie_btc_order = engine.place_order("charlie", "BTC", OrderType.MARKET, 
                                          OrderSide.BUY, Decimal('0.05'))
    
    # Wait for order matching
    await asyncio.sleep(1)
    
    # Display order books
    print("\nCurrent Order Books:")
    for symbol in ["BTC", "ETH"]:
        order_book = engine.get_order_book(symbol)
        print(f"\n{symbol} Order Book:")
        print(f"  Buy Orders: {len(order_book['buy'])}")
        for order in order_book['buy'][:3]:
            print(f"    {order['quantity']} @ ${order['price']}")
        print(f"  Sell Orders: {len(order_book['sell'])}")
        for order in order_book['sell'][:3]:
            print(f"    {order['quantity']} @ ${order['price']}")
    
    # Display portfolios
    print("\nCurrent Portfolios:")
    for user in users:
        portfolio = engine.get_portfolio(user)
        print(f"\n{user.title()}'s Portfolio:")
        print(f"  Total Value: ${portfolio.total_value:.2f}")
        for asset, balance in portfolio.balances.items():
            if balance > 0:
                print(f"  {asset}: {balance}")
    
    # Generate and display report
    print("\nGenerating Trading Report...")
    report = engine.generate_trading_report()
    
    print(f"Total Trades Executed: {report['total_trades']}")
    print(f"Active Orders: {report['active_orders']}")
    
    if 'total_volume' in report:
        print(f"Total Trading Volume: ${report['total_volume']:.2f}")
        print(f"Average Trade Size: ${report['average_trade_size']:.2f}")
    
    # Generate market analysis plots
    print("\nGenerating market analysis plots...")
    engine.plot_market_analysis()
    
    # Wait a bit more and show final state
    await asyncio.sleep(3)
    
    print("\nFinal Market Prices:")
    for symbol, asset in engine.assets.items():
        print(f"{symbol}: ${asset.price:.2f}")
    
    # Stop market simulation
    engine.stop_market_simulation()
    
    print("\nTrading engine demo completed!")
    print("Check 'trading_analysis.png' for market visualization")
    print(f"Database saved as: {engine.db_path}")
    
    return engine

def main():
    """Main function to run the trading engine demo"""
    asyncio.run(demo_trading_engine())

if __name__ == "__main__":
    main()