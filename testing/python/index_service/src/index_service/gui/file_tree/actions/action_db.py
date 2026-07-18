from sqlalchemy import Integer, String, JSON, DateTime
from sqlalchemy.orm import DeclarativeBase, mapped_column, Mapped
from datetime import datetime
from beartype.typing import Optional, Any


class Base(DeclarativeBase):
    pass


class OperationRow(Base):
    __tablename__ = "operations"

    id: Mapped[int] = mapped_column(Integer, primary_key=True, autoincrement=True)
    kind: Mapped[str] = mapped_column(String, nullable=False)
    action_type: Mapped[str] = mapped_column(String, nullable=False)
    action_data: Mapped[Any] = mapped_column(JSON, nullable=False)
    status: Mapped[str] = mapped_column(String, nullable=False)
    execution_hash: Mapped[str] = mapped_column(String, nullable=False, unique=True)
    started_at: Mapped[Optional[datetime]] = mapped_column(DateTime(timezone=True),
                                                           nullable=True)
    finished_at: Mapped[Optional[datetime]] = mapped_column(DateTime(timezone=True),
                                                            nullable=True)
    reverted_at: Mapped[Optional[datetime]] = mapped_column(DateTime(timezone=True),
                                                            nullable=True)
