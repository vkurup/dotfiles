dropdb libya_elections && \
    createdb libya_elections && \
    python manage.py migrate && \
    fab testing load_infrastructure:local && \
    python manage.py create_audit_test_data && \
    python manage.py create_help_desk_groups && \
    python manage.py create_reporting_api_test_data --yes-delete-my-data --num-centers 100 --num-registration-dates 40 && \
    python manage.py generate_reporting_api_reports && \
    python manage.py createsuperuser --username vkurup --email vkurup@caktusgroup.com
