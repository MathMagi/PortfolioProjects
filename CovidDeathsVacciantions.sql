Select *
FROM Portfolio..CovidDeaths
Where continent is not null
ORDER by 3,4

--Select *
--FROM Portfolio..CovidVaccinations
--ORDER by 3,4

--Select Data we are going to be using
Select Location, date, total_cases, new_cases,total_deaths,population
FROM Portfolio..CovidDeaths
order by 1,2

-- Looking at Total Cases vs Total Deaths
-- Shows the likelihood of dying when contracting Covid by country
Select Location, date, total_cases, total_deaths, (total_deaths/total_cases)*100 as DeathPercentage
FROM Portfolio..CovidDeaths
WHERE location like '%states%'
order by 1,2

-- Looking at total cases vs population
-- Percentage of Population has contracted Covid
Select Location, date, population, total_cases, (total_cases/population)*100 as PercentPopulationInfected
FROM Portfolio..CovidDeaths
WHERE location like '%states%'
order by 1,2


--Looking at total deaths vs population
-- Percentage of deaths per population
Select Location, date, population, total_deaths, (total_deaths/population)*100 as MortalityRate
FROM Portfolio..CovidDeaths
WHERE location like '%states%'
order by 1,2

--Looking at Countries with Highest Infection Rate compared to Population
Select Location, population, MAX(total_cases) as HighestInfectionCount, MAX((total_cases/population))*100 as PercentPopulationInfected
FROM Portfolio..CovidDeaths
--WHERE location like '%states%'
GROUP by Location, Population 
order by PercentPopulationInfected desc


--Looking at Countries with Highest	Death count per Population
Select location, MAX(cast(total_deaths as int)) as TotalDeathCount
FROM Portfolio..CovidDeaths
--WHERE location like '%states%'
WHERE continent is not null
Group BY location
order by TotalDeathCount desc

-- Let's break things down by continent

----Showing continents with highest death count
--Select continent, MAX(cast(total_deaths as int)) as TotalDeathCount
--FROM Portfolio..CovidDeaths
--WHERE continent is not null
--Group BY continent
--order by TotalDeathCount desc

-- This is the correct version of the above 
Select location, MAX(cast(total_deaths as int)) as TotalDeathCount
FROM Portfolio..CovidDeaths
--WHERE location like '%states%'
WHERE continent is null
Group BY location
order by TotalDeathCount desc

--Showing continents with highest death count
Select location, MAX(cast(total_deaths as int)) as TotalDeathCount
FROM Portfolio..CovidDeaths
--WHERE location like '%states%'
WHERE continent is null
Group BY location
order by TotalDeathCount desc

--Contient Percentage of population infected
Select Location, population, MAX(total_cases) as HighestInfectionCount, MAX((total_cases/population))*100 as PercentPopulationInfected
FROM Portfolio..CovidDeaths
Where continent is null
GROUP by Location, Population 
order by PercentPopulationInfected desc

--Global Numbers

Select SUM(new_cases) as total_cases, sum(cast(new_deaths as int)) as total_deaths, (sum(cast(new_deaths as int))/sum(new_cases))*100 as DeathPercentage
From Portfolio..CovidDeaths
Where continent is not null
--Group by date
order by 1,2


-- Join data sets 
-- Looking at total vacs vs population
--Use CTE
With PopvsVac (Continent, Location, date, population, new_vacciantions, RollingPeopleVaccinated)
as 
(
Select dea.continent, dea.location, dea.date, dea.population, vac.new_vaccinations,
SUM(cast(vac.new_vaccinations as int)) OVER (partition by dea.location order by dea.location, dea.date) as RollingPeopleVaccinated
--,(RollingPeopleVaccinated/dea.population)*100
From Portfolio..CovidDeaths dea
Join Portfolio..CovidVaccinations vac
	on dea.location = vac.location 
	and dea.date = vac.date
Where dea.continent is not null
--Order by 1,2,3
)
Select *, (RollingPeopleVaccinated/population)*100
From PopvsVac


-- Creating View to store data for later visualizations

Create View PercentPopulationVaccinated as
Select dea.continent, dea.location, dea.date, dea.population, vac.new_vaccinations
, SUM(CONVERT(int,vac.new_vaccinations)) OVER (Partition by dea.Location Order by dea.location, dea.Date) as RollingPeopleVaccinated
--, (RollingPeopleVaccinated/population)*100
From Portfolio..CovidDeaths dea
Join Portfolio..CovidVaccinations vac
	On dea.location = vac.location
	and dea.date = vac.date
where dea.continent is not null 
