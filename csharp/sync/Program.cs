using sync;

var builder = WebApplication.CreateBuilder(args);

var app = builder.Build();

app.MapGet("/fizzbuzz", () => Interpreter.RunJson(Interpreter.FizzBuzz));

app.Run("http://127.0.0.1:4000");
