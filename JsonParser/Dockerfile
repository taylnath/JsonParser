FROM mcr.microsoft.com/dotnet/runtime:5.0 AS base
WORKDIR /app

FROM mcr.microsoft.com/dotnet/sdk:5.0 AS build
WORKDIR /src
COPY ["JsonParser/JsonParser.fsproj", "JsonParser/"]
RUN dotnet restore "JsonParser/JsonParser.fsproj"
COPY . .
WORKDIR "/src/JsonParser"
RUN dotnet build "JsonParser.fsproj" -c Release -o /app/build

FROM build AS publish
RUN dotnet publish "JsonParser.fsproj" -c Release -o /app/publish

FROM base AS final
WORKDIR /app
COPY --from=publish /app/publish .
ENTRYPOINT ["dotnet", "JsonParser.dll"]
