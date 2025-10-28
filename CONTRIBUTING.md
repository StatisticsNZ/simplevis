Thank you for your interest in contributing to this project.
These guidelines explain how to contribute code, documentation, or feedback to repositories under the Statistics NZ GitHub Organization.

# 1. Ways to contribute

You can contribute by:

* Reporting bugs or issues
* Suggesting enhancements
* Submitting code (new features/ bug fixes)
* Improving documentation (README, Wiki, tutorials)

# 2. Workflow
a. Fork and branch
* Fork the repository to your own GitHub account
* Create a new branch for your code development:

```
git checkout -b feature/<short-description>
```

* Use clear, descriptive branch names (e.g., ```bugfix/fix-sql-query```)

b. Code Standards
* Follow the coding standards for the relevant programming language (R, Phyton, SAS, SQL, JavaScript, etc.)
* Write clean, well-documented code
* Include comments for complex logic
* Add or update test where possible

c. Commits
Use clear, descriptive commit messages:
```
[git command]: "Short summary"
```
Examples: 
```
git commit -m "Fix: corrected null handling in SQL query"

git commit -m "feat: added R function for time series interpolation"
```

d. Pull Request (PRs)
* Targer the ```uat``` or ```dev``` branch for testing and integration
* PRs to ```main/master``` (production) are only made by mainteners/owners after quality assurance testing
* Each PR should include:
  - Reference to related issue(s) (e.g., ```Fixes #42```)
  - Contain a clear description of the change
  - Pass automated checks (linting, unit tests)

# 3. Documentation
* Update the ```README.md``` if your change affects usage
* Add Roxygen2 style comments to explain new functions, workflows, or data transformations
* Follow the repository structure for documentation

# 4. Licensing and Data Protection
* All contributors must comply with the **Crown Copyright** license unless otherwisew specified.
* Do not commit sensitive data, credentials, or unpublished statistics
* Ensure that any third-party libraries or code comply with the approved licenses listed in the repository

# 5. Getting help
If you are unsure about how to contribute, please:

* Open a discussion in the respository's **Issues** section
* Contact the repository maintainers
* If repository maintainers not available, contact the admninistrator: githubadmin@stats.govt.nz
