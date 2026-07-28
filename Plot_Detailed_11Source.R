# Load required libraries
library(tidyverse)

# 1. Dataset populated with 11-category Detailed Source adjusted model findings
df_11 <- tibble::tribble(
  ~group, ~term, ~estimate, ~conf.low, ~conf.high, ~p.value,
  
  # Detailed Source of Care (11 categories, Pharmacy ref)
  "Detailed Source of Care\n(Ref: Pharmacy)", "Public hospital", 1.72, 1.35, 2.19, 0.000,
  "Detailed Source of Care\n(Ref: Pharmacy)", "Private hospital/clinic", 2.70, 1.81, 4.03, 0.000,
  "Detailed Source of Care\n(Ref: Pharmacy)", "Public clinic/health centre", 1.43, 1.06, 1.93, 0.020,
  "Detailed Source of Care\n(Ref: Pharmacy)", "Private physician", 1.34, 1.10, 1.64, 0.004,
  "Detailed Source of Care\n(Ref: Pharmacy)", "Community health worker", 1.86, 1.18, 2.92, 0.007,
  "Detailed Source of Care\n(Ref: Pharmacy)", "Mobile/outreach clinic", 0.14, 0.02, 1.23, 0.076,
  "Detailed Source of Care\n(Ref: Pharmacy)", "Shop/market/street", 1.88, 1.34, 2.64, 0.000,
  "Detailed Source of Care\n(Ref: Pharmacy)", "Traditional practitioner", 1.06, 0.89, 1.27, 0.526,
  "Detailed Source of Care\n(Ref: Pharmacy)", "Relative/friend", 0.44, 0.06, 3.28, 0.421,
  "Detailed Source of Care\n(Ref: Pharmacy)", "Other", 3.13, 2.34, 4.19, 0.000,
  
  # Covariates
  "Child Sex\n(Ref: Female)", "Male", 1.09, 0.96, 1.23, 0.199,
  
  "Child Age Group\n(Ref: 0–5 months)", "6–11 months", 1.17, 0.91, 1.51, 0.222,
  "Child Age Group\n(Ref: 0–5 months)", "12–23 months", 1.12, 0.89, 1.41, 0.342,
  "Child Age Group\n(Ref: 0–5 months)", "24–35 months", 1.27, 1.01, 1.61, 0.043,
  "Child Age Group\n(Ref: 0–5 months)", "36–47 months", 1.09, 0.85, 1.38, 0.502,
  "Child Age Group\n(Ref: 0–5 months)", "48–59 months", 1.29, 1.00, 1.68, 0.054,
  
  "Child Illness Status\n(Ref: Single/No)", "Multiple illnesses", 2.75, 2.40, 3.15, 0.000,
  
  "Mother's Education\n(Ref: No education)", "Primary", 0.91, 0.70, 1.16, 0.438,
  "Mother's Education\n(Ref: No education)", "Secondary", 0.95, 0.74, 1.21, 0.660,
  "Mother's Education\n(Ref: No education)", "Higher", 0.99, 0.74, 1.32, 0.928,
  
  "Residence\n(Ref: Rural)", "Urban", 0.90, 0.75, 1.09, 0.288,
  
  "Wealth Quintile\n(Ref: Poorest)", "Poor", 1.02, 0.84, 1.24, 0.821,
  "Wealth Quintile\n(Ref: Poorest)", "Middle", 1.02, 0.83, 1.26, 0.839,
  "Wealth Quintile\n(Ref: Poorest)", "Richer", 1.28, 1.03, 1.59, 0.024,
  "Wealth Quintile\n(Ref: Poorest)", "Richest", 1.21, 0.94, 1.56, 0.130,
  
  "Family Size\n(Ref: Small 1–3)", "Medium (4–6)", 0.83, 0.68, 1.01, 0.068,
  "Family Size\n(Ref: Small 1–3)", "Large (7+)", 0.88, 0.69, 1.11, 0.274,
  
  "Crowding Category\n(Ref: Low 1–2)", "Medium (3–4)", 1.16, 1.00, 1.35, 0.054,
  "Crowding Category\n(Ref: Low 1–2)", "High (5+)", 1.17, 0.90, 1.51, 0.242
)

# 2. Process factor levels and formatting
group_order <- unique(df_11$group)
term_order <- rev(unique(df_11$term)) # Reverse so top item in list stays at top of plot

df_11 <- df_11 %>%
  mutate(
    sig = ifelse(p.value < 0.05, "p < 0.05", "p ≥ 0.05"),
    sig = factor(sig, levels = c("p ≥ 0.05", "p < 0.05")),
    label = sprintf("%.2f (%.2f–%.2f)", estimate, conf.low, conf.high),
    group = factor(group, levels = group_order),
    term = factor(term, levels = term_order)
  )

# 3. Generate Forest Plot
p_11 <- ggplot(df_11, aes(x = estimate, y = term)) +
  # Null reference line at OR = 1.0
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey50", linewidth = 0.5) +
  
  # Error bars (Horizontal 95% CIs)
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high, color = sig), 
                 height = 0, linewidth = 0.6) +
  
  # Points (Circle for p >= 0.05, Diamond for p < 0.05)
  geom_point(aes(color = sig, shape = sig), size = 2.8) +
  
  # Text annotations on right side [AOR (95% CI)]
  geom_text(aes(x = 5.2, label = label), hjust = 0, size = 3.2, color = "black") +
  
  # Facet vertically by variable group (with strip boxes on the left)
  facet_grid(group ~ ., scales = "free_y", space = "free_y", switch = "y") +
  
  # Axis limits & Log scale
  scale_x_log10(
    breaks = c(0.1, 0.25, 0.5, 1, 2, 4),
    limits = c(0.04, 9.5),
    labels = c("0.1", "0.25", "0.5", "1", "2", "4")
  ) +
  
  # Aesthetics mappings
  scale_color_manual(
    name = "Significance",
    values = c("p ≥ 0.05" = "#34495e", "p < 0.05" = "#c0392b")
  ) +
  scale_shape_manual(
    name = "Significance",
    values = c("p ≥ 0.05" = 16, "p < 0.05" = 18)
  ) +
  
  # Labels and captions
  labs(
    title = "Factors Associated with Antibiotic Use Among Children Under 5 Years in Bangladesh",
    subtitle = "Detailed Source Model (11 Categories) — Adjusted Odds Ratios from Multivariable Logistic Regression",
    x = "Adjusted Odds Ratio (log scale)",
    y = NULL,
    caption = "Error bars represent 95% confidence intervals.\nDiamond (red) = statistically significant (p < 0.05).\nAdjusted for child sex, age, illness, maternal education, residence, wealth, family size, and crowding."
  ) +
  
  # Theme customization matching reference layout
  theme_bw(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 13),
    plot.subtitle = element_text(hjust = 0.5, color = "grey30", size = 10),
    plot.caption = element_text(size = 7.5, color = "grey40", hjust = 1, margin = margin(t = 10)),
    
    axis.title.x = element_text(size = 9.5, margin = margin(t = 8)),
    axis.text.y = element_text(size = 8.5, color = "black"),
    axis.text.x = element_text(size = 8.5, color = "black"),
    
    # Left facet box styling
    strip.position = "left",
    strip.background = element_rect(fill = "#eaeded", color = "black", linewidth = 0.5),
    strip.text.y.left = element_text(angle = 0, face = "bold", size = 8, color = "#2c3e50"),
    
    # Outer box around each facet panel
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    panel.grid.major.x = element_line(color = "grey92", linewidth = 0.4),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 9),
    legend.text = element_text(size = 9),
    
    plot.margin = margin(t = 12, r = 25, b = 10, l = 10)
  )

# Display plot
print(p_11)

# Save high-resolution PNG (Height set to 11 inches to comfortably accommodate all 11 detailed source levels)
ggsave("Forest_Plot_Detailed_11Source.png", plot = p_11, width = 11, height = 11, dpi = 300)