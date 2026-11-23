# Item gradient (Figure 2, Table A3): the 2023 design effect on items outside the
# naturalisation battery. Same contrast as Figure 1 (self-administered arms
# pooled minus CAPI) for 7-point items asked in the CAPI arm and in split A of
# the self-administered arms:
#   - Placebo, same format: life-domain importance (li, 1 unimportant - 7 very
#     important), without normative content (church/religion kept separate)
#   - Placebo, other format: institutional trust (pt)
#   - Normatively loaded comparison: attitudes to foreigners (ma, mp)
#   - Perceived lifestyle differences of groups (mg): descriptive, Table A3 only
# These are all the 7-point attitude batteries with those universes, so the set is
# defined by the instrument and not by the results.
# If the placebo items moved as much as mn01/mn02, a general response-style shift
# would explain the descent effect. Also reports the shift into the top category
# (7), the pattern the civic mn items show in Figure 1. Figure 2 shows the share
# 5-7 gaps; Table A3 has the full results.

source(here::here("code", "helpers.R"))

battery <- tribble(
  ~set, ~items,
  "Naturalisation: descent", c("mn01", "mn02"),
  "Naturalisation: other", c("mn03", "mn04", "mn05", "mn06", "mn07", "mn08", "mn09"),
  "Placebo: life domains", c("li01", "li02", "li03", "li04", "li05", "li07", "li08"),
  "Placebo: religion domains", c("li09", "li10"),
  "Placebo: institutional trust", c("pt01", "pt02", "pt03", "pt04", "pt06", "pt07", "pt08",
                                    "pt09", "pt10", "pt11", "pt12", "pt14", "pt15", "pt19",
                                    "pt20"),
  "Attitudes to foreigners", c("ma01b", "ma02", "ma03", "ma04", "mp02", "mp04", "mp05",
                               "mp06", "mp07", "mp10", "mp12"),
  "Perceived lifestyle differences", c("mg01", "mg02", "mg03", "mg04", "mg05", "mg06")
) |>
  unnest_longer(items, values_to = "item") |>
  mutate(set = fct_inorder(set))

placebo_items <- setdiff(battery$item, mn_items)

d23 <- read_rds(clean("allbus_mn.rds")) |>
  filter(year == 2023) |>
  left_join(
    read_dta(raw("ZA8833_v1-0-0.dta"), col_select = c(respid, all_of(placebo_items))) |>
      mutate(across(all_of(placebo_items), as_item)),
    by = "respid"
  )

w <- boot_weights(d23, "region")
capi <- d23$arm == "CAPI"

gaps <- battery |>
  expand_grid(outcome = c("share_ge5", "share_7", "mean")) |>
  mutate(res = map2(item, outcome, \(i, o) {
    x <- d23[[i]]
    y <- switch(o, mean = x, share_ge5 = as.numeric(x >= 5), share_7 = as.numeric(x == 7))
    boot_summary(boot_mean(y, d23$self_admin, w) - boot_mean(y, capi, w)) |>
      mutate(capi = boot_mean(y, capi, w)[1])
  })) |>
  unnest(res)

save_tab(gaps, "tableA3_gradient")

# Largest placebo gap vs the descent items
gaps |>
  filter(outcome == "share_ge5") |>
  summarise(max_abs_gap = max(abs(estimate)), mean_gap = mean(estimate), .by = set) |>
  mutate(across(where(is.numeric), \(x) round(100 * x, 1))) |>
  print()

# Figure 2: the gradient ----------------------------------------------------------

# Blocks regrouped to follow the argument in the text
gradient_groups <- c(
  "Naturalisation:\ndescent", "Naturalisation:\nother criteria", "Demands on\nforeigners",
  "Foreigners:\nnegative\nstatements", "Foreigners:\npositive\nstatements",
  "Institutional\ntrust", "Life domains"
)

fig_gradient <- gaps |>
  filter(outcome == "share_ge5", !str_starts(item, "mg")) |>
  mutate(
    group = case_when(
      item %in% c("mn01", "mn02") ~ gradient_groups[1],
      str_starts(item, "mn") ~ gradient_groups[2],
      str_starts(item, "ma") ~ gradient_groups[3],
      item %in% c("mp02", "mp04", "mp06", "mp07") ~ gradient_groups[4],
      str_starts(item, "mp") ~ gradient_groups[5],
      str_starts(item, "pt") ~ gradient_groups[6],
      str_starts(item, "li") ~ gradient_groups[7]
    ) |> factor(levels = gradient_groups),
    label = coalesce(mn_labels[item], placebo_labels[item]),
    label = fct_reorder(label, estimate),
    descent = item %in% c("mn01", "mn02")
  ) |>
  ggplot(aes(100 * estimate, label, colour = descent)) +
  geom_vline(xintercept = 0, colour = "grey60") +
  geom_pointrange(aes(xmin = 100 * lower, xmax = 100 * upper), size = 0.2) +
  geom_text(aes(x = 31, label = scales::percent(capi, accuracy = 1)),
            hjust = 1, size = 2.6, colour = "grey40") +
  facet_grid(rows = vars(group), scales = "free_y", space = "free_y", switch = "y") +
  scale_x_continuous(breaks = seq(-15, 25, 5), limits = c(-18, 31)) +
  scale_colour_manual(values = c(`TRUE` = arm_colours[["Sequential"]], `FALSE` = "grey20"),
                      guide = "none") +
  labs(
    x = "Difference to CAPI (percentage points)",
    y = NULL,
    caption = str_c("Share 5-7 = agree, trust or important. Points: pooled self-administered arms minus CAPI, 95% bootstrap CIs.\n",
                     "Right column: share 5-7 in the CAPI arm.")
  ) +
  theme_paper() +
  theme(
    strip.placement = "outside",
    strip.text.y.left = element_text(angle = 0, hjust = 0, vjust = 0.5, face = "bold"),
    panel.spacing.y = unit(3, "mm"),
    panel.grid.major.y = element_blank()
  )

save_fig(fig_gradient, "fig2_gradient", height = 198)
