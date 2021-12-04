open System
open System.IO
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.Logging
open Microsoft.Extensions.DependencyInjection
open Giraffe
open LibExecution

let run e =
    fun (next: HttpFunc) (ctx: HttpContext) ->
        let fizzbuzz = Interpreter.runJSON e
        text fizzbuzz next ctx

let webApp =
    choose [ GET
             >=> choose [ route "/fizzbuzz" >=> run Interpreter.fizzbuzz ] ]

let configureApp (app: IApplicationBuilder) = app.UseGiraffe webApp

let configureServices (services: IServiceCollection) = services.AddGiraffe() |> ignore

let configureLogging (builder: ILoggingBuilder) =
    builder
        .AddFilter(fun l -> l >= LogLevel.Debug)
        .AddConsole()
        .AddDebug()
    |> ignore

[<EntryPoint>]
let main args =
    let contentRoot = Directory.GetCurrentDirectory()

    Host
        .CreateDefaultBuilder(args)
        .ConfigureWebHostDefaults(fun webHostBuilder ->
            webHostBuilder
                .UseUrls(sprintf "http://127.0.0.1:4000")
                .UseContentRoot(contentRoot)
                .Configure(Action<IApplicationBuilder> configureApp)
                .ConfigureServices(configureServices)
                .ConfigureLogging(configureLogging)
            |> ignore)
        .Build()
        .Run()

    0
