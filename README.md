r
## (K-3) Prepare distance/correlation matrix

# ایجاد ماتریس A با ابعاد k × (n*n) - صفر اولیه
A <- matrix(0, nrow = k, ncol = n * n)

# حلقه روی هر بعد (i از 1 تا k)
for (i in 1:k) {
  if (control$types[i] != "factor") {
    # برای متغیرهای عددی: فاصله اقلیدسی با تابع dist
    # dist(fit$scaledx[, i]) فاصله بین تمام جفت نقاط در بعد i را می‌دهد
    # as.matrix() تبدیل به ماتریس می‌کند، as.numeric() به بردار یک‌بعدی تبدیل می‌کند
    A[i, ] <- as.numeric(as.matrix(dist(fit$scaledx[, i])))
    
  } else {
    # برای متغیرهای دسته‌ای (factor): فاصله هامینگ
    # outer: مقایسه هر جفت نقطه با عملگر "!=" → TRUE اگر متفاوت باشند
    tmp <- outer(fit$scaledx[, i], fit$scaledx[, i], "!=")
    
    # تبدیل ماتریس منطقی به عددی (TRUE → 1, FALSE → 0)
    class(tmp) <- "numeric"
    
    # ذخیره به صورت بردار در سطر i ام از A
    A[i, ] <- tmp
  }
}
