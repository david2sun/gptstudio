# 1) LM Studio 主机地址
lmstudio_api_url <- function() {
  Sys.getenv("LMSTUDIO_HOST", "http://localhost:11535")
}

# 2) 拼接 LM Studio 的 /v1/<task> 路径 
lmstudio_set_task <- function(task) {
  lmstudio_api_url() |>
    request() |>
    req_url_path_append("v1") |>
    req_url_path_append(task)
}

# 3) 检查 LM Studio 是否可用
lmstudio_is_available <- function(verbose = FALSE) {
  request <- lmstudio_api_url() |>
    request()

  check_value <- logical(1)

  rlang::try_fetch(
    {
      response <- req_perform(request) |>
        resp_body_string()

      if (verbose) cli::cli_alert_success(response)
      check_value <- TRUE
    },
    error = function(cnd) {
      if (inherits(cnd, "httr2_failure")) {
        if (verbose) cli::cli_alert_danger("Couldn't connect to LM Studio at {.url {lmstudio_api_url()}}. Is it running there?") # nolint
      } else {
        if (verbose) cli::cli_alert_danger(cnd)
      }
      check_value <- FALSE
    }
  )

  invisible(check_value)
}

# 4) 发送“聊天”请求到 LM Studio
#    LM Studio 的端点是 /v1/chat/completions
lmstudio_chat <- function(model, messages, stream = TRUE, shiny_session = NULL, user_prompt = NULL) {
  body <- list(
    model = model,
    messages = messages,
    stream = stream
  )

  # 拼成 /v1/chat/completions
  request <- lmstudio_set_task("chat") |>
    req_url_path_append("completions") |>
    req_body_json(data = body)

  if (stream) {
    # 复用您原先的 parser 与流处理逻辑
    parser <- OllamaStreamParser$new(
      session = shiny_session,
      user_prompt = user_prompt
    )

    # 这里可复用 ollama_perform_stream()，
    # 只要其对 JSON Stream 的解析逻辑符合 LM Studio 的输出格式即可
    ollama_perform_stream(
      request = request,
      parser = parser
    )

    # 取出最后一行
    last_line <- parser$lines[[length(parser$lines)]]
    # 插入到最后的 message 里
    last_line$message <- list(
      role = "assistant",
      content = parser$value
    )

    last_line
  } else {
    request |>
      req_perform() |>
      resp_body_json()
  }
}
