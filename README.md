# 🥤 Kiwi Bubbles: A Pricing Analytics Story  

Imagine it’s the late 1990s. Kiwi, a well-known soft drink brand, is doing fine with its classic **Kiwi Regular**. But across the aisle, Mango has launched **Mango Bubbles**, and consumers love it.  

Kiwi’s managers start to wonder: *What if we launch our own carbonated version — Kiwi Bubbles? Would it succeed? How should we price it? And how would Mango respond?*  

This project is the journey of answering those questions with **data-driven pricing analytics**.  

---

## 📖 How We Approached the Problem  

### Step 1: Listening to the Data  
We fit a **multinomial logit model**. A few highlights:  

- Consumers showed a **negative price coefficient** (β ≈ -2.4, p < 0.01), confirming demand falls with higher prices.  
- **Base preference (intercept)** for Kiwi Regular was higher than Mango Bubbles, but Kiwi Bubbles gained traction once introduced.  
- At average prices:  
  - Own-price elasticity of Kiwi Bubbles: **-3.1** (highly price sensitive).  
  - Cross-price elasticity KB ↔ MB: **+1.8** (strong substitution).  
  - Cross-price elasticity KB ↔ KR: **+0.4** (mild cannibalization).  

**Insight:** Kiwi Bubbles competes more with Mango than with Kiwi’s own product. 

---

### Step 2: Realizing Not All Shoppers Are the Same  
Of course, not every consumer behaves alike. Some are loyal to the Kiwi brand, others are bubble enthusiasts. So we used **K-means clustering on demographic data** to uncover hidden segments.  

This segmentation told a richer story:  
- One group valued *bubbles* above all.  
- Another group cared mostly about *price*.  
- A loyal cluster stuck with Kiwi no matter what.  

Elasticities differed sharply:  
- For bubble lovers, cross-elasticity KB ↔ MB rose to **+2.3**.  
- For loyalists, KR’s demand hardly shifted (elasticity near 0).  

**Insight:** Segmentation uncovered *who to target* and *why KB creates value*.  

---

### Step 3: Testing “What If” Worlds  
Next, we ran **profit simulations** and simulated profits under two scenarios:  

- **No KB launch**: Optimal KR price = **$1.55**. Kiwi profit ≈ **$620**, Mango profit ≈ **$510**.  
- **With KB launch**: Optimal KR price = **$1.50**, KB price = **$1.67**. Kiwi profit ≈ **$810**, Mango profit ≈ **$430**.  

**Insight:** Even after cannibalization, Kiwi Bubbles boosted total profit by ~30%.   

- In a world where Kiwi doesn’t launch Kiwi Bubbles, the best move is to keep Kiwi Regular priced optimally.  
- In a world where Kiwi does launch Kiwi Bubbles, we optimized prices for both products together.  

Comparing profits across these scenarios revealed the trade-offs:  
- Some cannibalization of Kiwi Regular was inevitable.  
- But the added market share from Mango and the willingness-to-pay of bubble-loving segments could make Kiwi Bubbles worthwhile.  

---

### Step 4: Facing the Competitor  
But the story doesn’t end with Kiwi’s decision. Mango wouldn’t just sit still at $1.43.  

We simulated a **pricing war**:  

- Starting prices: KR = $1.50, KB = $1.67, MB = $1.43.  
- After 4 iterations of best responses, prices converged to:  
  - KR = **$1.48**, KB = **$1.64**, MB = **$1.39**.  
- Equilibrium profits: Kiwi ≈ **$780**, Mango ≈ **$410**.  

**Insight:** Kiwi Bubbles still held a strategic edge, even after competitive adjustment.  

This iterative process converged to an **equilibrium price set**, showing the realistic battlefield.  

The lesson? Even when competitors fight back, Kiwi Bubbles can hold its ground — if positioned smartly.  

---

## 🔑 What We Learned  

- **Elasticities** showed Kiwi Bubbles was more of a threat to Mango than to Kiwi itself.  
- **Segmentation** revealed where to position the product: target bubble-lovers, not price-sensitive shoppers.  
- **Profit analysis** justified the launch, proving Kiwi Bubbles could expand total profit.  
- **Competitive simulation** reminded us to never assume rivals stay still.  

---

## 🛠️ Behind the Scenes  

- **Tools**: R (`mlogit`, `gmnl`, `data.table`)  
- **Methods**: Logit models, elasticity calculation, segmentation, profit maximization, pricing equilibrium simulation  
- **Files in this repo**:  
  - `Pricing_analytics_PS2.pdf` – full project write-up  
  - `topic_2(1).R` – code for estimation & simulations  
  - `kiwi_bubbles.csv` – dataset (purchase history)  

---

## 🌟 Why This Project Matters  

This project isn’t just about soda. It’s about **how companies make smarter pricing decisions**:  
1. Estimate how people react to prices.  
2. Recognize that not all customers are the same.  
3. Simulate profits before making a risky launch.  
4. Anticipate competitors’ countermoves.  

Kiwi Bubbles might be fictional — but the lessons apply to real pricing battles every day.  
